#![feature(iterator_flatten, specialization, try_from)]

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

    let mut screenbox = ScreenBox { width: width.into(), height: height.into(), writer: &mut stdout };
    let screeneventbox = ScreenBox { width: width.into(), height: height.into(), writer: () };

    screen_state.render(&mut screenbox);
    screenbox.flush().unwrap();

    for c in stdin.events() {
        let evt = c.unwrap();
        screen_state.handle_event(&screeneventbox, &evt);
        match evt {
            Event::Key(Key::Char('q')) => break,
            Event::Mouse(me) => {
                match me {
                    MouseEvent::Press(_, x, y) => {
                        write!(screenbox, "{}x", termion::cursor::Goto(x, y)).unwrap();
                    },
                    _ => (),
                }
            }
            _ => {}
        }

        screen_state.render(&mut screenbox);
        screenbox.flush().unwrap();
    }
}

struct ScreenBox<T> {
    width: usize,
    height: usize,
    writer: T,
}

impl<'a, 'b, T: Write> Write for ScreenBox<T> {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.writer.write(bytes)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

use std::convert::TryInto;

trait ToSigned
    where
        Self: std::convert::TryInto<<Self as ToSigned>::Signed>,
        <Self as ToSigned>::Signed: std::convert::TryInto<Self>
    {
    type Signed;
}
impl ToSigned for usize { type Signed = isize; }

enum Change<T: Copy + ToSigned>
    {
    By(<T as ToSigned>::Signed),
    To(T),
}

use Change::*;

impl<T: Copy + ToSigned> Change<T> 
    where
        <T as std::convert::TryInto<<T as ToSigned>::Signed>>::Error: std::fmt::Debug,
        <<T as ToSigned>::Signed as std::convert::TryInto<T>>::Error: std::fmt::Debug,
        T: std::ops::Add<Output = T>,
        <T as ToSigned>::Signed: std::ops::Add<Output = <T as ToSigned>::Signed>,
        <T as ToSigned>::Signed: Copy,
    {
    fn change(&self, val: T) -> T {
        match *self {
            Change::By(dv) => (val.try_into().unwrap() + dv).try_into().unwrap(),
            Change::To(v) => v,
        }
    }
}

impl<T: Copy> ScreenBox<T> {
    fn shrink_copy(&self, w: Change<usize>, h: Change<usize>) -> ScreenBox<T> {
        ScreenBox { width: w.change(self.width), height: h.change(self.height), writer: self.writer }
    }
}

impl<'a, T> ScreenBox<&'a T> {
    fn shrink(&self, w: Change<usize>, h: Change<usize>) -> ScreenBox<&T> {
        ScreenBox { width: w.change(self.width), height: h.change(self.height), writer: self.writer }
    }
}

impl<'a, T> ScreenBox<&'a mut T> {
    fn shrink(&mut self, w: Change<usize>, h: Change<usize>) -> ScreenBox<&mut T> {
        ScreenBox { width: w.change(self.width), height: h.change(self.height), writer: self.writer }
    }
}


trait ScreenObject {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>);
    fn handle_event(&mut self, w: &ScreenBox<()>, e: &Event);
}


struct ScreenState {
    file_list: FileList,
}
impl ScreenObject for ScreenState {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>) {
        write!(w, "{}{}{}", clear::All, termion::cursor::Goto(1, 1), termion::cursor::Hide).unwrap();
        self.file_list.render(w);
    }
    fn handle_event(&mut self, b: &ScreenBox<()>, e: &Event) {
        self.file_list.handle_event(b, e);
    }
}

struct ScreenList<T: ItemScreenObject> {
    txt: String,
    skip: usize,
    selected: usize,
    items: Vec<T>,
}

impl<T: ItemScreenObject> ScreenObject for ScreenList<T> {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>) {
        let mut first = true;
        let wh = w.height;
        write!(w, "{}\r\n", wh).unwrap();
        for (i, f) in self.items.iter().enumerate() {
            if i < self.skip { continue }
            if i - self.skip >= w.height - 1 { break }

            if first { first = false; }
            else { write!(w, "\r\n").unwrap(); }

            if i == self.selected {
                write!(w, "{}", style::Invert).unwrap();
            }
            f.render(&mut w.shrink(By(0), To(1)), i == self.selected);
            if i == self.selected {
                write!(w, "{}", style::NoInvert).unwrap();
            }
        }
    }
    fn handle_event(&mut self, b: &ScreenBox<()>, e: &Event) {
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
    fn move_selected(&mut self, d: isize, b: &ScreenBox<()>) {
        let len = self.items.len() as isize;
        let sel = ((self.selected as isize) + d) % len;

        self.selected = if sel < 0 { sel + len } else { sel } as usize;

        if self.selected < self.skip {
            self.skip = self.selected;
        } else if self.selected - self.skip >= b.height {
            self.txt = format!("skip adjusted: {}, {}, {}", self.selected, self.skip, b.height);
            self.skip = self.selected - (b.height - 1);
        }

    }
}

trait ItemScreenObject {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>, selected: bool);
    fn handle_event<W: Write>(&mut self, b: &ScreenBox<W>, e: &Event, selected: bool);
}
/*default impl<T: ItemScreenObject> ScreenObject for T {
    fn render<W: Write>(&self, w: &mut W) {
        self.render(w, false)
    }
    fn handle_event<W: Write>(&self, e: &Event) {
        self.handle_event(e, false)
    }
}
default impl<T: ScreenObject> ItemScreenObject for T {
    fn render<W: Write>(&self, w: &mut W, selected: bool) {
        self.render(w)
    }
    fn handle_event<W: Write>(&self, e: &Event, selected: bool) {
        self.handle_event(e)
    }
}*/


struct FileList {
    path: PathBuf,
    files: Vec<FileTreeItem>,
    screen_list: ScreenList<FileItem>,
    txt: String,
}

impl FileList {
    fn new<P: AsRef<Path>>(p: &P) -> FileList {
        let files = FileList::get_files(p);
        let mut fl = FileList {
            path: p.as_ref().to_path_buf(),
            files: files,
            screen_list: ScreenList {
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

impl ScreenObject for FileList {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>) {
        write!(w, "{} {}\r\n", self.path.to_string_lossy(), self.txt).unwrap();
        self.screen_list.render(&mut w.shrink(By(0), By(-1)));
    }
    fn handle_event(&mut self, b: &ScreenBox<()>, e: &Event) {
        let b = &b.shrink_copy(By(0), By(-1));
        match e {
            &Event::Key(Key::Char('\n')) => {
                let res = FileTreeItem::walk_command_children(&mut self.files, self.screen_list.selected, &TreeCommand::SetExpanded(None));
                self.update_list();
                self.txt = format!("{:?}", res);
            },
            _ => {
                self.screen_list.handle_event(b, e)
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
    Continue(usize),
    Done,
}

struct FileItem {
    entry: DirEntry,
    expanded: bool,
    indent: u32,
}

impl ItemScreenObject for FileItem {
    fn render<W: Write>(&self, w: &mut ScreenBox<&mut W>, selected: bool) {
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

        write!(w, "{}{} {} {}", indent, sel_prefix, type_prefix, path).unwrap();
    }
    fn handle_event<W: Write>(&mut self, _b: &ScreenBox<W>, e: &Event, selected: bool) {
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

    fn walk_command(&mut self, index: usize, cmd: &TreeCommand) -> CommandResponse {
        if index == 0 {
            // command applies to me!
            return match cmd {
                TreeCommand::SetExpanded(exp) => {
                    if self.is_dir() {
                        self.expanded = match exp {
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

    fn walk_command_children(children: &mut Vec<FileTreeItem>, index: usize, cmd: &TreeCommand) -> CommandResponse {
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

