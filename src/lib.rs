#![feature(proc_macro_hygiene)]

extern crate plex;

use crate::{UnptxToken::*};

use plex::{lexer, parser};

use std::io::{BufRead};

#[derive(Debug)]
pub enum UnptxToken {
  Whitespace,
  Comment,
  DotVersion,
  DotTarget,
  DotAddressSize,
  DotVisible,
  DotEntry,
  Semi,
  LParen,
  RParen,
  LCurl,
  RCurl,
  // instrs
  Ret,
  BarSync,
  // literals
  IntLit(i64),
  FloatLit(i64, i64),
  Ident(String),
}

lexer! {
  fn next_token(text: 'a) -> UnptxToken;
  r"[ \t\r\n]+" => UnptxToken::Whitespace,
  r"//.*" => UnptxToken::Comment,
  r"\.version" => UnptxToken::DotVersion,
  r"\.target" => UnptxToken::DotTarget,
  r"\.address_size" => UnptxToken::DotAddressSize,
  r"\.visible" => UnptxToken::DotVisible,
  r"\.entry" => UnptxToken::DotEntry,
  r";" => UnptxToken::Semi,
  r"\(" => UnptxToken::LParen,
  r"\)" => UnptxToken::RParen,
  r"{" => UnptxToken::LCurl,
  r"}" => UnptxToken::RCurl,
  r"ret" => UnptxToken::Ret,
  r"bar\.sync" => UnptxToken::BarSync,
  r"[0-9]+" => UnptxToken::IntLit(0),
  r"[0-9]+\.[0-9]*" => UnptxToken::FloatLit(0, 0),
  r"[_A-Za-z][_0-9A-Za-z]*" => UnptxToken::Ident(text.to_owned()),
}

pub struct UnptxLineLexer<'s> {
  buf:  &'s str,
  eol:  bool,
}

impl<'s> UnptxLineLexer<'s> {
  pub fn new(buf: &'s str) -> UnptxLineLexer<'s> {
    UnptxLineLexer{
      buf,
      eol:  false,
    }
  }
}

impl<'s> Iterator for UnptxLineLexer<'s> {
  type Item = UnptxToken;

  fn next(&mut self) -> Option<UnptxToken> {
    if self.eol {
      return None;
    }
    loop {
      match next_token(self.buf) {
        Some((tok, next_buf)) => {
          self.buf = next_buf;
          match tok {
            UnptxToken::Whitespace => {
              continue;
            }
            UnptxToken::Comment => {
              self.eol = true;
              return None;
            }
            _ => {
              return Some(tok);
            }
          }
        }
        None => {
          self.eol = true;
          return None;
        }
      }
    }
  }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Version {
  Ptx_3_2,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Target {
  Sm_3_5,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AddressSize {
  _64,
}

#[derive(Debug)]
pub enum Directive {
  Version(Version),
  Target(Target),
  AddressSize(AddressSize),
}

#[derive(Debug)]
pub enum Attribute {
  Visible,
  Entry,
}

#[derive(Debug)]
pub enum UnptxLine {
  Empty,
  Directive(Directive),
  KernelEntry,
}

#[derive(Debug)]
pub struct Identifier(pub String);

parser! {
  fn parse_line_(UnptxToken, ());
  line: UnptxLine {
    => UnptxLine::Empty,
    directive[d] => UnptxLine::Directive(d),
    attributes[attrs] Ident(id) LParen RParen => {
      // TODO
      UnptxLine::KernelEntry
    }
  }
  directive: Directive {
    DotVersion => Directive::Version(Version::Ptx_3_2),
    DotTarget => Directive::Target(Target::Sm_3_5),
    DotAddressSize => Directive::AddressSize(AddressSize::_64),
  }
  attributes: Vec<Attribute> {
    attribute[a] => {
      vec![a]
    }
    attributes[mut attrs] attribute[a] => {
      attrs.push(a);
      attrs
    }
  }
  attribute: Attribute {
    DotVisible => Attribute::Visible,
    DotEntry => Attribute::Entry,
  }
}

pub fn parse_line<L: Iterator<Item=UnptxToken>>(line_lexer: L) -> Result<UnptxLine, (Option<(UnptxToken, ())>, &'static str)> {
  parse_line_(line_lexer.map(|tok| (tok, ())))
}

pub struct UnptxLines<R> {
  reader:   R,
}

impl<R> UnptxLines<R> {
  pub fn new(reader: R) -> UnptxLines<R> {
    UnptxLines{reader}
  }
}

impl<R: BufRead> Iterator for UnptxLines<R> {
  type Item = UnptxLine;

  fn next(&mut self) -> Option<UnptxLine> {
    let mut buf = String::new();
    if self.reader.read_line(&mut buf).is_err() {
      return None;
    }
    if buf.is_empty() {
      return None;
    }
    match parse_line(UnptxLineLexer::new(&buf)) {
      Err(_) => panic!("unptx: syntax error"),
      Ok(line) => Some(line),
    }
  }
}

#[derive(Default)]
pub struct UnptxBuilder {
  version:  Option<Version>,
  target:   Option<Target>,
  addrsize: Option<AddressSize>,
  _state:   (),
}

impl UnptxBuilder {
  pub fn with_lines<I: Iterator<Item=UnptxLine>>(mut self, mut lines: I) -> Result<Unptx, ()> {
    // TODO
    unimplemented!();
  }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Unptx {
  pub version:  Version,
  pub target:   Target,
  pub addrsize: AddressSize,
}
