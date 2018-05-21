#![feature(match_default_bindings, iterator_flatten, specialization, try_from)]

#[macro_use]
extern crate structopt;
extern crate termion;
extern crate ignore;

use termion::event::{Key, Event, MouseEvent};
use termion::input::{TermRead, MouseTerminal};
use termion::raw::IntoRawMode;
use termion::screen::{AlternateScreen};
use termion::{style, clear};
use termion::terminal_size;

use ignore::{WalkBuilder, DirEntry};

//use core::clone::Clone;
use std::io::{Write, stdout, stdin};
use std::iter;
use std::path::{Path, PathBuf};
//use std::marker::PhantomData;
//use std::borrow::Borrow;
use std::iter::repeat;

use structopt::StructOpt;


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

    let stdin = stdin();
    let stdout = AlternateScreen::from(MouseTerminal::from(stdout().into_raw_mode().unwrap()));
    let mut stdout = stdout.lock();

    let mut screen_state = ScreenState{
        file_list: FileList::new(&opts.path),
    };

    let mut screenbox = ScreenBox::new((width.into(), height.into());

    screen_state.render(&mut screenbox);
    screenbox.flush().unwrap();

    for c in stdin.events() {
        let evt = c.unwrap();
        screen_state.handle_event(&evt);
        match evt {
            Event::Key(Key::Char('q')) => break,
            Event::Mouse(me) => {
                match me {
                    MouseEvent::Press(_, x, y) => {
                        screenbox.draw_str_at((x as _, y as _), Style::default(), "x".to_string());
                    },
                    _ => (),
                }
            }
            _ => {}
        }

        screen_state.render(&mut screenbox);
        //screenbox.flush().unwrap();
    }
}


#[derive(Copy, Clone)]
enum Color {
    Indexed(u16),
    RGB(u8, u8, u8),
}

#[derive(Copy, Clone)]
struct Style {
    fg: Color,
    bg: Color,

}

impl Style {
    fn default() -> Style {
        Style {
            fg: Color::Indexed(1),
            bg: Color::Indexed(0),
        }
    }
}

#[derive(Clone)]
struct StyledText {
    style: Style,
    text: Rc<String>,
    width: u32,
}

impl StyledText {
    fn new(style: Style, text: String) -> StyledText {
        StyledText { style, text, width: unicode::width(&text) }
    }

}

impl PaintableWidget for StyledText {
    fn size(&self) -> (u32, u32) {
        (self.width, 1)
    }

    fn draw_into<R: RawPaintable>(&self, target: R, pos: (u32, u32)) {
        target.draw_text_at(pos, self.clone())
    }
}


#[derive(Clone)]
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
    fn draw_text_at(&mut self, x: u32, str: StyledText) {
        // first, find the index of the text that x is in
        let start_index = match self.texts.iter()
            .scan(0 as u32, |i, &txt| Some(*i + txt.width))
            .position(|i| i <= x) {
                Some(i) => i,
                None => return
            };


    }
}

impl PaintableWidget for Line {
    fn size(&self) -> (u32, u32) {
        (self.texts.iter().map(|t| t.width).sum(), 1)
    }

    fn draw_into<R: RawPaintable>(&self, target: R, pos: (u32, u32)) {
        let mut width_so_far = 0;
        for t in self.texts.iter() {
            t.draw_into(target, (x + width_so_far, pos.1));
            width_so_far += t.width;
        }
    }
    fn draw_delta_into<R: RawPaintable>(&mut self, target: R, (x, y): (u32, u32)) {
        let mut width_so_far = 0;
        for t in self.texts.iter() {
            t.draw_delta_into(target, (pos.0 + width_so_far, pos.1));
            width_so_far += t.width;
        }
    }
}

struct ScreenBox {
    size: (u32, u32),
    lines: Vec<Line>,
}

impl ScreenBox {
    fn new(size: (u32, u32)) -> ScreenBox {
        ScreenBox {
            size,
            lines: repeat(Line::new(size.0)).take(size.1).collect(),
        }
    }
}

trait RawPaintable {
    fn size(&self) -> (u32, u32);
    fn draw_text_at(&mut self, pos: (u32, u32), text: StyledText);
}

trait FancyPaintable: RawPaintable {
    fn draw_str_at(&mut self, pos: (u32, u32), style: Style, str: String) {
        self.draw_text_at(pos, StyledText::new(style, str));
    }
}
impl<T: RawPaintable> FancyPaintable for T {}

trait PaintableWidget {
    fn size(&self) -> (u32, u32);
    fn draw_into<R: RawPaintable>(&self, target: R, pos: (u32, u32));
    fn draw_delta_into<R: RawPaintable>(&mut self, target: R, pos: (u32, u32)) {
        // if there isn't a faster implementation, just call draw_into
        self.draw_into(target, pos)
    }
}

impl RawPaintable for ScreenBox {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn draw_text_at(&mut self, pos: (u32, u32), text: StyledText) {
        if pos.1 < self.size.1 && pos.1 as usize < self.lines.len() {
            self.lines[pos.1 as usize].draw_text_at(pos.0, text)
        }
    }
}

impl PaintableWidget for ScreenBox {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn draw_into<R: RawPaintable>(&self, target: R, pos: (u32, u32)) {
        for (i, l) in self.lines.iter().enumerate() {
            l.draw_into(target, (pos.0, pos.1 + i as u32))
        }
    }
    fn draw_delta_into<R: RawPaintable>(&mut self, target: R, pos: (u32, u32)) {
        for (i, l) in self.lines.iter().enumerate() {
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
        self.file_list.draw_delta_into((0, 0), self.screenbox);
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
    fn render(&self) {
        self.screenbox.draw_str_at((0, 0), Style::default(),
            format!("{}", self.screenbox.size.1));
        for (i, f) in self.items.iter().enumerate() {
            if i as u32 < self.skip { continue }
            if i as u32 - self.skip >= self.screenbox.size.1 - 1 { break }

            //if i == self.selected {
                //write!(w, "{}", style::Invert).unwrap();
            //}
            f.render(i as u32 == self.selected);
            //if i as u32 == self.selected {
                //write!(w, "{}", style::NoInvert).unwrap();
            //}
        }
    }

}

impl<T: ItemScreenObject> ScreenObject for ScreenList<T> {
    fn handle_event(&mut self, e: &Event) {
        let b = &b.shrink_copy(By(0), By(-1));
        match e {
            &Event::Key(Key::Up) |
            &Event::Key(Key::Char('k')) => self.move_selected(-1, b),
            &Event::Key(Key::Down) |
            &Event::Key(Key::Char('j')) => self.move_selected(1, b),
            _ => { },
        }
    }
}

impl<T: ItemScreenObject> ScreenList<T> {
    fn move_selected(&mut self, d: isize, b: &ScreenBox) {
        let len = self.items.len() as isize;
        let sel = ((self.selected as isize) + d) % len;

        self.selected = if sel < 0 { sel + len } else { sel } as u32;

        if self.selected < self.skip {
            self.skip = self.selected;
        } else if self.selected - self.skip >= b.size.1 {
            self.txt = format!("skip adjusted: {}, {}, {}", self.selected, self.skip, b.size.1);
            self.skip = self.selected - (b.size.1 - 1);
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
        let mut fl = FileList {
            screenbox,
            path: p.as_ref().to_path_buf(),
            files: files,
            screen_list: ScreenList {
                screenbox: ScreenBox::new((screenbox.size.0, screenbox.size.1 - 1)),
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
        println!("items: {}", items.len());
        self.screen_list.items = items;
    }
}

impl FileList {
    fn render(&self) {

        write!(w, "{} {}\r\n", self.path.to_string_lossy(), self.txt).unwrap();
        self.screen_list.screenbox.draw_delta_into(self.screenbox, (0, 1));
    }
}

impl ScreenObject for FileList {
    fn handle_event(&mut self, e: &Event) {
        let b = &b.shrink_copy(By(0), By(-1));
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
        let i = iter::once(FileItem{ entry: self.entry.clone(), expanded: self.expanded, indent });

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

