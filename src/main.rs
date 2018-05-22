#![feature(collections_range, iterator_flatten, specialization, try_from)]

#[macro_use]
extern crate structopt;
extern crate termion;
extern crate ignore;
extern crate unicode_width;

use termion::event::{Key, Event, MouseEvent};
use termion::input::{TermRead, MouseTerminal};
use termion::raw::IntoRawMode;
use termion::screen::{AlternateScreen};
use termion::terminal_size;

use ignore::{WalkBuilder, DirEntry};

//use core::clone::Clone;
use std::io::{Write, stdout, stdin};
use std::iter;
use std::path::{Path, PathBuf};
//use std::marker::PhantomData;
//use std::borrow::Borrow;
use std::iter::repeat;
use std::rc::Rc;
use std::fmt::{Display, Formatter};
use std::ops::RangeBounds;

use structopt::StructOpt;
use unicode_width::{UnicodeWidthStr, UnicodeWidthChar};


#[derive(StructOpt, Debug)]
#[structopt(name = "fuzzytree")]
struct Opts {
    /// Path to start from
    #[structopt(default_value=".")]
    path: String,
}

fn main() {
    let opts = Opts::from_args();

    let (width, height) = terminal_size().unwrap();
    println!("Terminal size is {}, {}", width, height);
    //let width = 20 as u32;
    //let height = 15 as u32;

    let stdin = stdin();
    let stdout = MouseTerminal::from(stdout().into_raw_mode().unwrap());
    let stdout = AlternateScreen::from(stdout);
    let mut stdout = stdout.lock();

    let mut screen_state = ScreenState{
        screenbox: ScreenBox::new((width as _, height as _)),
        file_list: FileList::new(&opts.path, ScreenBox::new((width as _, (height-1) as _))),
    };

    write!(stdout, "{}", termion::clear::All).unwrap();

    //let mut screenbox = ScreenBox::new((width.into(), height.into()));
    let mut terminal = Terminal {
        size: (width as _, height as _),
        current_style: Style::default(),
        w: stdout
    };


    screen_state.screenbox.draw_into(&mut terminal, (0, 0));
    terminal.w.flush().unwrap();
    //screen_state.render(&mut screenbox);
    //screenbox.flush().unwrap();

    for c in stdin.events() {
        let evt = c.unwrap();
        match evt {
            Event::Key(Key::Char('q')) => break,
            Event::Mouse(me) => {
                match me {
                    MouseEvent::Press(_, x, y) => {
                        screen_state.screenbox.draw_str_at(((x-1) as _, (y-1) as _), Style::default(), "x".to_string());
                    },
                    _ => (),
                }
            }
            _ => {
                screen_state.handle_event(&evt);
            }
        }

        screen_state.screenbox.draw_delta_into(&mut terminal, (0, 0));
        terminal.w.flush().unwrap();
        //screen_state.render(&mut screenbox);
        //screenbox.flush().unwrap();
    }
}


struct Terminal<W: Write> {
    size: (u32, u32),
    current_style: Style,
    w: W,
}

impl<W: Write> RawPaintable for Terminal<W> {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText) {
        write!(self.w, "{}{}{}", termion::cursor::Goto(1 + pos.0 as u16, 1 + pos.1 as u16), text.style, text.text).unwrap();
    }
}


#[derive(Debug, Copy, Clone)]
enum Color {
    Default,
    Indexed(u16),
    RGB(u8, u8, u8),
}

impl Color {
    fn termion_color(&self) -> Box<termion::color::Color> {
        match *self {
            Color::Default => Box::new(termion::color::Reset),
            Color::Indexed(i) => Box::new(termion::color::AnsiValue(i as _)),
            Color::RGB(r, g, b) => Box::new(termion::color::Rgb(r, g, b)),
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Style {
    fg: Color,
    bg: Color,

}

impl Style {
    fn default() -> Style {
        Style {
            fg: Color::Default,
            bg: Color::Default,
        }
    }
}

impl Display for Style {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let fg = self.fg.termion_color();
        let bg = self.bg.termion_color();
        fg.write_fg(f)?;
        bg.write_bg(f)
    }
}

#[derive(Debug, Clone)]
struct StyledText {
    style: Style,
    text: Rc<String>,
    width: u32,
}

/// Width sensitive slice. A and B are counted in cells.
fn width_slice(txt: &str, a: usize, b: usize) -> &str {
    let mut width_so_far = 0;
    let mut found_start = false;
    let mut start_index = 0;
    let mut end_index = txt.len();
    for (i, c) in txt.char_indices() {
        if !found_start {
            if width_so_far == a {
                start_index = i;
                found_start = true;
            }
            else if width_so_far > a {
                // TODO: Error!
            }
        }
        if found_start {
            if width_so_far >= b {
                end_index = i;
                break;
            }
        }

        width_so_far += UnicodeWidthChar::width(c).unwrap_or(0);
    }

    if !found_start {
        return "";
    }

    //println!("\rwidth_slice: {:?}[{}..{}] : {}..{}", txt, a, b, start_index, end_index);
    unsafe { txt.get(start_index..end_index).unwrap() }
}

impl StyledText {
    fn new(style: Style, text: String) -> StyledText {
        let width = UnicodeWidthStr::width(&text as &str);
        StyledText { style, text: Rc::new(text), width: width as _ }
    }

    fn slice<R: RangeBounds<usize>>(&self, r: R) -> StyledText where String: std::ops::Index<R> {
        // TODO: This should respect character widths...
        let a = match r.start() {
            std::ops::Bound::Included(i) => *i,
            std::ops::Bound::Excluded(i) => i+1,
            std::ops::Bound::Unbounded => 0,
        };
        let b = match r.end() {
            std::ops::Bound::Included(i) => *i+1,
            std::ops::Bound::Excluded(i) => *i,
            std::ops::Bound::Unbounded => self.text.len(),
        };
        let n = StyledText::new(self.style, width_slice(&self.text, a, b).to_string());
        //println!("\n\rStyledText::slice: {}..{}  {:?}", a, b, n);
        n
    }
}

impl PaintableWidget for StyledText {
    fn size(&self) -> (u32, u32) {
        (self.width, 1)
    }

    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32)) {
        target.draw_text_at(pos, &self)
    }
}


#[derive(Debug, Clone)]
struct Line {
    // There are no gaps between these.
    texts: Vec<StyledText>,
}

impl Line {
    fn new(width: u32) -> Line {
        Line {
            texts: vec![
                StyledText {
                    style: Style::default(),
                    text: Rc::new(" ".repeat(width as usize)),
                    width: width,
                }
            ],
        }
    }
}

impl Line {
    fn draw_text_at(&mut self, x: u32, txt: &StyledText) {
        let txt_end = x + txt.width;

        let mut t_column = 0;
        let mut start_found = false;
        let mut start_sliced = None;
        let mut start_index = 0;
        let mut end_index = self.texts.len()-1;
        let mut end_sliced = None;
        for (i, t) in self.texts.iter().enumerate() {
            let t_end = t_column + t.width;
            if !start_found {
                if t_end > x {
                    start_index = i;
                    if t_column < x {
                        start_sliced = Some(t.slice(..(x as usize)-(t_column as usize)));
                    }
                    start_found = true;
                }
            }
            if start_found {
                if t_end >= txt_end {
                    end_index = i;
                    if txt_end < t_end {
                        end_sliced = Some(t.slice((txt_end as usize)-(t_column as usize)..));
                    }
                    break;
                }
            }
            t_column = t_end;
        }

        // start is out of bounds
        if !start_found { return }

        let repl = [start_sliced, Some(txt.clone()), end_sliced];
        let repl = repl.iter().flatten().cloned();

    //if *txt.text == "x" {
        //println!("\n\rLine.draw_text_at {}, {:?}: {}..{}", x, txt.text, start_index, end_index);
        //println!("\rLine.draw_text_at before: {:?}", self.texts);
    //}
        self.texts.splice(start_index..end_index+1, repl);
    //if *txt.text == "x" {
        //println!("\rLine.draw_text_at after: {:?}", self.texts);
    //}

        //self.texts = new_texts;
    }
}

impl PaintableWidget for Line {
    fn size(&self) -> (u32, u32) {
        (self.texts.iter().map(|t| t.width).sum(), 1)
    }

    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32)) {
        let mut width_so_far = 0;
        for t in self.texts.iter() {
            t.draw_into(target, (pos.0 + width_so_far, pos.1));
            width_so_far += t.width;
        }
    }
    fn draw_delta_into<R: RawPaintable>(&mut self, target: &mut R, (x, y): (u32, u32)) {
        let mut width_so_far = 0;
        for t in self.texts.iter_mut() {
            t.draw_delta_into(target, (x + width_so_far, y));
            width_so_far += t.width;
        }
    }
}

#[derive(Debug)]
struct ScreenBox {
    size: (u32, u32),
    lines: Vec<Line>,
}

impl ScreenBox {
    fn new(size: (u32, u32)) -> ScreenBox {
        ScreenBox {
            size,
            lines: repeat(Line::new(size.0)).take(size.1 as _).collect(),
        }
    }
}

trait RawPaintable {
    fn size(&self) -> (u32, u32);
    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText);
}

trait FancyPaintable: RawPaintable {
    fn draw_str_at(&mut self, pos: (u32, u32), style: Style, str: String) {
        self.draw_text_at(pos, &StyledText::new(style, str));
    }
}
impl<T: RawPaintable> FancyPaintable for T {}

trait PaintableWidget {
    fn size(&self) -> (u32, u32);
    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32));
    fn draw_delta_into<R: RawPaintable>(&mut self, target: &mut R, pos: (u32, u32)) {
        // if there isn't a faster implementation, just call draw_into
        self.draw_into(target, pos)
    }
}

impl RawPaintable for ScreenBox {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText) {
        if pos.1 < self.size.1 && (pos.1 as usize) < self.lines.len() {
            self.lines[pos.1 as usize].draw_text_at(pos.0, text)
        }
    }
}

impl PaintableWidget for ScreenBox {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32)) {
        for (i, l) in self.lines.iter().enumerate() {
            l.draw_into(target, (pos.0, pos.1 + i as u32))
        }
    }
    fn draw_delta_into<R: RawPaintable>(&mut self, target: &mut R, pos: (u32, u32)) {
        for (i, l) in self.lines.iter_mut().enumerate() {
            l.draw_delta_into(target, (pos.0, pos.1 + i as u32))
        }
    }
}




trait ScreenObject {
    fn handle_event(&mut self, e: &Event);
}


struct ScreenState {
    screenbox: ScreenBox,
    file_list: FileList,
}
impl ScreenObject for ScreenState {
    //fn render(&self, w: &mut ScreenBox) {
        //write!(w, "{}{}{}", clear::All, termion::cursor::Goto(1, 1), termion::cursor::Hide).unwrap();
        //self.file_list.render(w);
    //}
    fn handle_event(&mut self, e: &Event) {
        self.file_list.handle_event(e);
        self.file_list.screenbox.draw_delta_into(&mut self.screenbox, (0, 0));
    }
}

struct ScreenList<T: ItemScreenObject> {
    screenbox: ScreenBox,
    txt: String,
    skip: u32,
    selected: u32,
    items: Vec<T>,
}

impl<T: ItemScreenObject> ScreenList<T> {
    fn render(&mut self) {
        let height = self.screenbox.size.1;
        self.screenbox.draw_str_at((0, 0), Style::default(),
            format!("{}", height));
        for (i, f) in self.items.iter().enumerate() {
            if (i as u32) < self.skip { continue }
            if (i as u32) - self.skip >= self.screenbox.size.1 - 1 { break }

            //if i == self.selected {
                //write!(w, "{}", style::Invert).unwrap();
            //}
            //f.render(i as u32 == self.selected);
            //if i as u32 == self.selected {
                //write!(w, "{}", style::NoInvert).unwrap();
            //}
        }
    }

}

impl<T: ItemScreenObject> ScreenObject for ScreenList<T> {
    fn handle_event(&mut self, e: &Event) {
        match e {
            &Event::Key(Key::Up) |
            &Event::Key(Key::Char('k')) => self.move_selected(-1),
            &Event::Key(Key::Down) |
            &Event::Key(Key::Char('j')) => self.move_selected(1),
            _ => { },
        }
    }
}

impl<T: ItemScreenObject> ScreenList<T> {
    fn move_selected(&mut self, d: isize) {
        let len = self.items.len() as isize;
        let sel = ((self.selected as isize) + d) % len;

        self.selected = if sel < 0 { sel + len } else { sel } as u32;

        if self.selected < self.skip {
            self.skip = self.selected;
        } else if self.selected - self.skip >= self.screenbox.size.1 {
            self.txt = format!("skip adjusted: {}, {}, {}", self.selected, self.skip, self.screenbox.size.1);
            self.skip = self.selected - (self.screenbox.size.1 - 1);
        }
    }
}

trait ItemScreenObject {
    fn handle_event(&mut self, e: &Event, selected: bool);
}


struct FileList {
    screenbox: ScreenBox,
    path: PathBuf,
    files: Vec<FileTreeItem>,
    screen_list: ScreenList<FileItem>,
    txt: String,
}

impl FileList {
    fn new<P: AsRef<Path>>(p: &P, screenbox: ScreenBox) -> FileList {
        let files = FileList::get_files(p);
        let size = screenbox.size;
        let mut fl = FileList {
            screenbox,
            path: p.as_ref().to_path_buf(),
            files: files,
            screen_list: ScreenList {
                screenbox: ScreenBox::new((size.0, size.1 - 1)),
                txt: "".to_string(),
                skip: 0,
                selected: 0,
                items: vec![],
            },
            txt: "".to_string(),
        };
        fl.update_list();
        fl
    }
/*    fn navigate<P: AsRef<Path>>(&mut self, p: P) {
        self.path = p.as_ref().to_path_buf();
        self.files = read_dir(p).unwrap().filter_map(|x| x.ok()).collect();
        self.selected = 0;
    }*/

    fn get_files<P: AsRef<Path>>(p: &P) -> Vec<FileTreeItem> {
        WalkBuilder::new(p)
            .max_depth(Some(1))
            .build()
            .skip(1) // first entry is always self
            .filter_map(|x| x.ok())
            .map(|x| FileTreeItem { entry: x, expanded: false, children: None })
            .collect()
    }

    fn update_list(&mut self) {
        let items: Vec<_> = self.files.iter().flat_map(|x| x.iter_file_items(0)).collect();
        //println!("items: {}", items.len());
        self.screen_list.items = items;
    }
}

impl FileList {
    fn render(&mut self) {
        //write!(w, "{} {}\r\n", self.path.to_string_lossy(), self.txt).unwrap();
        self.screenbox.draw_str_at((0, 0), Style::default(),
            format!("{} {}", self.path.to_string_lossy(), self.txt));
        self.screen_list.screenbox.draw_delta_into(&mut self.screenbox, (0, 1));
    }
}

impl ScreenObject for FileList {
    fn handle_event(&mut self, e: &Event) {
        match e {
            &Event::Key(Key::Char('\n')) => {
                let res = FileTreeItem::walk_command_children(&mut self.files, self.screen_list.selected, &TreeCommand::SetExpanded(None));
                self.update_list();
                self.txt = format!("{:?}", res);
            },
            _ => {
                self.screen_list.handle_event(e)
            },
        }
    }
}

enum TreeCommand {
    SetExpanded(Option<bool>),
}

#[derive(Debug)]
enum CommandResponse {
    // Contains total number of visible children walked over
    Continue(u32),
    Done,
}

struct FileItem {
    screenbox: ScreenBox,
    entry: DirEntry,
    expanded: bool,
    indent: u32,
}

impl FileItem {
    fn render(&mut self, selected: bool) {
        let sel_prefix = if selected { ">" } else { " " };
        let mut type_prefix = "-";
        let indent = "  ".repeat(self.indent as usize);
        let path = self.entry.path();
        let mut path = path.file_name()
            .map(|f| f.to_string_lossy().into_owned())
            .unwrap_or_else(|| format!("<no_filename {}>", path.display()));
            //path.strip_prefix(".").unwrap().to_string_lossy();

        if let Some(filetype) = self.entry.file_type() {
            if filetype.is_dir() {
                path = path + "/";
                type_prefix = if self.expanded { "v" } else { ">" };
            }

        }

        self.screenbox.draw_str_at((0, 0), Style::default(),
            format!("{}{} {} {}", indent, sel_prefix, type_prefix, path));
    }
}

impl ItemScreenObject for FileItem {
    fn handle_event(&mut self, e: &Event, selected: bool) {
        match e {
            _ => {
            },
        }
    }
}

struct FileTreeItem {
    entry: DirEntry,
    expanded: bool,
    children: Option<Vec<FileTreeItem>>,
}

impl FileTreeItem {
    fn is_dir(&self) -> bool {
        if let Some(filetype) = self.entry.file_type() {
            filetype.is_dir()
        } else {
            false
        }
    }

    fn iter_file_items<'a>(self: &'a Self, indent: u32) -> Box<Iterator<Item=FileItem> + 'a> {
        let i = iter::once(FileItem{ screenbox: ScreenBox::new((1, 1)), entry: self.entry.clone(), expanded: self.expanded, indent });

        if self.expanded {
            if let Some(children) = &self.children {
                return Box::new(i.chain(children.iter().flat_map(move |x| x.iter_file_items(indent + 1))));
            }
        }

        Box::new(i)
    }

    fn walk_command(&mut self, index: u32, cmd: &TreeCommand) -> CommandResponse {
        if index == 0 {
            // command applies to me!
            return match cmd {
                TreeCommand::SetExpanded(exp) => {
                    if self.is_dir() {
                        self.expanded = match *exp {
                            None => !self.expanded,
                            Some(val) => !!val,
                        };
                        if self.expanded && self.children.is_none() {
                           let children = FileList::get_files(&self.entry.path());
                           self.children = Some(children);
                        }
                    }
                    CommandResponse::Done
                },
            }
        }

        if self.expanded {
            if let Some(children) = &mut self.children {
                return match FileTreeItem::walk_command_children(children, index - 1, cmd) {
                    CommandResponse::Continue(cnt) => CommandResponse::Continue(cnt + 1),
                    CommandResponse::Done => CommandResponse::Done,
                };
            }
        }

        CommandResponse::Continue(1)
    }

    fn walk_command_children(children: &mut Vec<FileTreeItem>, index: u32, cmd: &TreeCommand) -> CommandResponse {
        let mut count = 0;

        for mut child in children {
            match child.walk_command(index - count, cmd) {
                CommandResponse::Continue(cnt) => count += cnt,
                CommandResponse::Done => return CommandResponse::Done,
            }
        }

        CommandResponse::Continue(count)
    }
}

