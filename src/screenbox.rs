
use std;
use termion;
use std::io::{Write};
use std::fmt::{Display, Formatter};
use std::iter::repeat;
use std::ops::RangeBounds;
use std::rc::Rc;

use unicode_width::{UnicodeWidthStr, UnicodeWidthChar};

pub struct Terminal<W: Write> {
    size: (u32, u32),
    current_style: Style,
    w: W,
}

impl<W: Write> Terminal<W> {
    pub fn new(mut w: W, size: (u32, u32)) -> Terminal<W> {
        let current_style = Style::default();
        write!(w, "{}", current_style).unwrap();
        Terminal {
            size,
            current_style,
            w,
        }
    }
    pub fn flush(&mut self) -> Result<(), std::io::Error> {
        self.w.flush()
    }
}

impl<W: Write> HasSize for Terminal<W> {
    fn size(&self) -> (u32, u32) {
        self.size
    }
}

impl<W: Write> RawPaintable for Terminal<W> {
    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText) {
        write!(self.w, "{}{}{}", termion::cursor::Goto(1 + pos.0 as u16, 1 + pos.1 as u16), text.style, text.text).unwrap();
    }
}


#[derive(Debug, Copy, Clone)]
pub enum Color {
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

bitfield!{
    #[derive(Copy, Clone)]
    pub struct StyleAttrs(u16);
    impl Debug;
    bold, set_bold: 0;
    italic, set_italic: 1;
    faint, set_faint: 2;
    framed, set_framed: 3;
    invert, set_invert: 4;
    underline, set_underline: 5;
}

pub enum StyleAttr {
    Bold,
    Italic,
    Faint,
    Framed,
    Invert,
    Underline,
}

impl StyleAttr {
    fn isset_in(&self, attrs: &StyleAttrs) -> bool {
        match self {
            StyleAttr::Bold => attrs.bold(),
            StyleAttr::Italic => attrs.italic(),
            StyleAttr::Faint => attrs.faint(),
            StyleAttr::Framed => attrs.framed(),
            StyleAttr::Invert => attrs.invert(),
            StyleAttr::Underline => attrs.underline(),
        }
    }

    fn set_to_in(&self, to: bool, attrs: &mut StyleAttrs) {
        match self {
            StyleAttr::Bold => attrs.set_bold(to),
            StyleAttr::Italic => attrs.set_italic(to),
            StyleAttr::Faint => attrs.set_faint(to),
            StyleAttr::Framed => attrs.set_framed(to),
            StyleAttr::Invert => attrs.set_invert(to),
            StyleAttr::Underline => attrs.set_underline(to),
        }
    }
}


#[derive(Debug, Copy, Clone)]
pub struct Style {
    fg: Color,
    bg: Color,
    attrs: StyleAttrs,
}

impl Style {
    pub fn set_fg(&self, c: Color) -> Style {
        Style{
            fg: c,
            bg: self.bg,
            attrs: self.attrs,
        }
    }

    pub fn set_bg(&self, c: Color) -> Style {
        Style{
            fg: self.fg,
            bg: c,
            attrs: self.attrs,
        }
    }

    pub fn isset(&self, a: StyleAttr) -> bool {
        a.isset_in(&self.attrs)
    }

    pub fn set(&self, a: StyleAttr) -> Style {
        let mut newattrs = self.attrs;
        a.set_to_in(true, &mut newattrs);
        Style {
            fg: self.fg,
            bg: self.bg,
            attrs: newattrs,
        }
    }

    pub fn clear(&self, a: StyleAttr) -> Style {
        let mut newattrs = self.attrs;
        a.set_to_in(false, &mut newattrs);
        Style {
            fg: self.fg,
            bg: self.bg,
            attrs: newattrs,
        }
    }

    pub fn toggle(&self, a: StyleAttr) -> Style {
        let mut newattrs = self.attrs;
        a.set_to_in(!a.isset_in(&self.attrs), &mut newattrs);
        Style {
            fg: self.fg,
            bg: self.bg,
            attrs: newattrs,
        }
    }
}

impl Default for Style {
    fn default() -> Style {
        Style {
            fg: Color::Default,
            bg: Color::Default,
            attrs: StyleAttrs(0),
        }
    }
}

impl Display for Style {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.fg.termion_color().write_fg(f)?;
        self.bg.termion_color().write_bg(f)?;
        write!(f, "{}", termion::style::Reset)?;
        if self.attrs.bold() { write!(f, "{}", termion::style::Bold)? }
        if self.attrs.italic() { write!(f, "{}", termion::style::Italic)? }
        if self.attrs.faint() { write!(f, "{}", termion::style::Faint)? }
        if self.attrs.framed() { write!(f, "{}", termion::style::Framed)? }
        if self.attrs.invert() { write!(f, "{}", termion::style::Invert)? }
        if self.attrs.underline() { write!(f, "{}", termion::style::Underline)? }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StyledText {
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
                panic!("Slice in the middle of a double-width char!");
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
    // txt.get(start_index..end_index).unwrap()
    unsafe { txt.get_unchecked(start_index..end_index) }
}

impl StyledText {
    pub fn new(style: Style, text: String) -> StyledText {
        if text.len() == 0 { panic!("Zero width StyledText"); }

        let width = UnicodeWidthStr::width(&text as &str);
        StyledText { style, text: Rc::new(text), width: width as _ }
    }

    fn slice<R: RangeBounds<usize>>(&self, r: R) -> StyledText where String: std::ops::Index<R> {
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

impl HasSize for StyledText {
    fn size(&self) -> (u32, u32) {
        (self.width, 1)
    }
}

impl PaintableWidget for StyledText {
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

impl HasSize for Line {
    fn size(&self) -> (u32, u32) {
        (self.texts.iter().map(|t| t.width).sum(), 1)
    }
}

impl PaintableWidget for Line {
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
pub struct ScreenBox {
    size: (u32, u32),
    lines: Vec<Line>,
}

impl ScreenBox {
    pub fn new(size: (u32, u32)) -> ScreenBox {
        ScreenBox {
            size,
            lines: repeat(Line::new(size.0)).take(size.1 as _).collect(),
        }
    }
}

pub trait RawPaintable: HasSize {
    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText);

    fn draw_str_at(&mut self, pos: (u32, u32), style: Style, str: String) {
        self.draw_text_at(pos, &StyledText::new(style, str));
    }

    fn clear_line(&mut self, pos: (u32, u32), style: Style) {
        let spaces = " ".repeat(self.size().0 as _);
        self.draw_str_at(pos, style, spaces);
    }
}

pub trait HasSize {
    fn size(&self) -> (u32, u32);
}

pub trait PaintableWidget: HasSize {
    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32));
    fn draw_delta_into<R: RawPaintable>(&mut self, target: &mut R, pos: (u32, u32)) {
        // if there isn't a faster implementation, just call draw_into
        self.draw_into(target, pos)
    }
}

impl RawPaintable for ScreenBox {
    fn draw_text_at(&mut self, pos: (u32, u32), text: &StyledText) {
        if pos.1 < self.size.1 && (pos.1 as usize) < self.lines.len() {
            self.lines[pos.1 as usize].draw_text_at(pos.0, text)
        }
    }
}

impl HasSize for ScreenBox {
    fn size(&self) -> (u32, u32) {
        self.size
    }
}

impl PaintableWidget for ScreenBox {
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

pub trait HasScreenbox {
    fn screenbox(&self) -> &ScreenBox;
    fn screenbox_mut(&mut self) -> &mut ScreenBox;
}

impl<T: HasScreenbox> HasSize for T {
    fn size(&self) -> (u32, u32) {
        self.screenbox().size
    }
}

impl<T: HasScreenbox> PaintableWidget for T {
    fn draw_into<R: RawPaintable>(&self, target: &mut R, pos: (u32, u32)) {
        let sb = self.screenbox();
        sb.draw_into(target, pos)
    }
    fn draw_delta_into<R: RawPaintable>(&mut self, target: &mut R, pos: (u32, u32)) {
        let sb = self.screenbox_mut();
        sb.draw_delta_into(target, pos)
    }
}

