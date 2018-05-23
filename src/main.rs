#![feature(collections_range, iterator_flatten, specialization, try_from)]

extern crate fuzzytree;
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

use std::io::{Write, stdout, stdin};
use std::iter;
use std::path::{Path, PathBuf};

use structopt::StructOpt;

use fuzzytree::screenbox::*;


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
    let stdout = MouseTerminal::from(stdout().into_raw_mode().unwrap());
    let stdout = AlternateScreen::from(stdout);
    let mut stdout = stdout.lock();

    let mut screen_state = ScreenState{
        screenbox: ScreenBox::new((width as _, height as _)),
        file_list: FileList::new(&opts.path, ScreenBox::new((width as _, (height-1) as _))),
    };
    screen_state.render();

    write!(stdout, "{}", termion::clear::All).unwrap();

    let mut terminal = Terminal::new(stdout, (width as _, height as _));

    screen_state.screenbox.draw_into(&mut terminal, (0, 0));
    terminal.flush().unwrap();

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
        terminal.flush().unwrap();
    }
}




trait ScreenObject {
    fn handle_event(&mut self, e: &Event);
}


struct ScreenState {
    screenbox: ScreenBox,
    file_list: FileList,
}
impl ScreenState {
    fn render(&mut self) {
        self.file_list.screenbox.draw_delta_into(&mut self.screenbox, (0, 0));
    }
}
impl ScreenObject for ScreenState {
    fn handle_event(&mut self, e: &Event) {
        self.file_list.handle_event(e);
        self.render();
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
    fn new(size: (u32, u32), items: Vec<T>) -> ScreenList<T> {
        let mut sl = ScreenList {
            screenbox: ScreenBox::new(size),
            txt: "".to_string(),
            skip: 0,
            selected: 0,
            items,
        };
        sl.render();
        sl
    }

    fn set_items(&mut self, items: Vec<T>) {
        //let old_items = self.items;
        let visible_end = (self.skip + self.screenbox.size().1) as usize;
        if self.items.len() > items.len() && items.len() < visible_end {
            // need to erase some lines from the end
            let erase_lines = std::cmp::min(self.items.len() - items.len(), visible_end - items.len()) as u32;
            for i in (self.items.len() as u32 - self.skip - erase_lines)..(self.items.len() as u32 + 1) {
                self.screenbox.clear_line((0, i), Style::default());
            }
            self.txt = format!("Erased: {}", erase_lines);
        }
        else {
            self.txt = format!("No erase: {}, {}, {}, {}", self.items.len(), items.len(), visible_end, self.screenbox.size().1);
        }
        self.items = items;
        self.items[self.selected as usize].set_selected(true);
        self.render();
    }
}

impl<T: ItemScreenObject> ScreenList<T> {
    fn render(&mut self) {
        let height = self.screenbox.size().1;
        self.screenbox.draw_str_at((0, 0), Style::default(),
            format!("{}, {}", height, self.txt));
        for (i, f) in self.items.iter_mut().enumerate() {
            if (i as u32) < self.skip { continue }
            if (i as u32) - self.skip >= self.screenbox.size().1 - 1 { break }

            //if i == self.selected {
                //write!(w, "{}", style::Invert).unwrap();
            //}
            //f.render(i as u32 == self.selected);
            f.draw_delta_into(&mut self.screenbox, (0, (i as u32 - self.skip + 1)));
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
        };
        self.render();
    }
}

impl<T: ItemScreenObject> ScreenList<T> {
    fn move_selected(&mut self, d: isize) {
        let len = self.items.len() as isize;
        let sel = ((self.selected as isize) + d) % len;

        let new_sel = if sel < 0 { sel + len } else { sel } as u32;

        if self.selected == new_sel { return }

        self.items[self.selected as usize].set_selected(false);
        self.selected = new_sel;
        self.items[self.selected as usize].set_selected(true);

        if self.selected < self.skip {
            self.skip = self.selected;
        } else if self.selected - self.skip >= self.screenbox.size().1 {
            self.txt = format!("skip adjusted: {}, {}, {}", self.selected, self.skip, self.screenbox.size().1);
            self.skip = self.selected - (self.screenbox.size().1 - 1);
        }
    }
}

trait ItemScreenObject: PaintableWidget {
    fn set_selected(&mut self, selected: bool);
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
        let size = screenbox.size();
        let mut fl = FileList {
            screenbox,
            path: p.as_ref().to_path_buf(),
            files: files,
            screen_list: ScreenList::new((size.0, size.1 - 1), vec![]),
            txt: "".to_string(),
        };
        fl.update_list();
        fl.render();
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
        let items: Vec<_> = self.files.iter().flat_map(|x| x.iter_file_items(self.screenbox.size().0, 0)).collect();
        //println!("items: {}", items.len());
        self.screen_list.set_items(items);
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
        };
        self.render();
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

        if let Some(filetype) = self.entry.file_type() {
            if filetype.is_dir() {
                path = path + "/";
                type_prefix = if self.expanded { "v" } else { ">" };
            }

        }

        let mut style = Style::default();
        if selected {
            style = style.set(StyleAttr::Invert);
        }
        self.screenbox.draw_str_at((0, 0), style,
            format!("{}{} {} {}", indent, sel_prefix, type_prefix, path));
    }
}

impl HasScreenbox for FileItem {
    fn screenbox(&self) -> &ScreenBox { &self.screenbox }
    fn screenbox_mut(&mut self) -> &mut ScreenBox { &mut self.screenbox }
}

impl ItemScreenObject for FileItem {
    fn set_selected(&mut self, selected: bool) {
        self.render(selected);
    }
    fn handle_event(&mut self, e: &Event, selected: bool) {
        match e {
            _ => {
            },
        };
        self.render(selected);
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

    fn iter_file_items<'a>(&'a self, width: u32, indent: u32) -> Box<Iterator<Item=FileItem> + 'a> {
        let mut me = FileItem{ screenbox: ScreenBox::new((width, 1)), entry: self.entry.clone(), expanded: self.expanded, indent };
        me.render(false);
        let i = iter::once(me);

        if self.expanded {
            if let Some(children) = &self.children {
                return Box::new(i.chain(children.iter().flat_map(move |x| x.iter_file_items(width, indent + 1))));
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

