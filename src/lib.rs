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
  DotFunc,
  Colon,
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
  Int2Lit(i64, i64),
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
  r"\.func" => UnptxToken::DotFunc,
  r":" => UnptxToken::Colon,
  r";" => UnptxToken::Semi,
  r"\(" => UnptxToken::LParen,
  r"\)" => UnptxToken::RParen,
  r"{" => UnptxToken::LCurl,
  r"}" => UnptxToken::RCurl,
  r"ret" => UnptxToken::Ret,
  r"bar\.sync" => UnptxToken::BarSync,
  r"[0-9]+" => {
    match text.parse() {
      Ok(x) => UnptxToken::IntLit(x),
      _ => panic!(),
    }
  }
  r"[0-9]+\.[0-9]+" => {
    let ts: Vec<_> = text.split(".").collect();
    assert_eq!(ts.len(), 2);
    match (ts[0].parse(), ts[1].parse()) {
      (Ok(x0), Ok(x1)) => UnptxToken::Int2Lit(x0, x1),
      _ => panic!(),
    }
  }
  r"[a-zA-Z][a-zA-Z0-9_]*" => UnptxToken::Ident(text.to_owned()),
  r"[_$%][a-zA-Z0-9_]+" => UnptxToken::Ident(text.to_owned()),
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
  Visible,
  Entry,
  Func,
}

#[derive(Debug)]
pub enum Inst {
  Ret,
  BarSync,
}

#[derive(Debug)]
pub enum UnptxLine {
  Empty,
  Directive(Directive),
  KernelDirective,
  Inst(Inst),
}

impl UnptxLine {
  pub fn is_empty(&self) -> bool {
    match self {
      &UnptxLine::Empty => true,
      _ => false,
    }
  }
}

//#[derive(Debug)]
//pub struct Identifier(pub String);

parser! {
  fn parse_line_(UnptxToken, ());
  line: UnptxLine {
    => UnptxLine::Empty,
    LCurl => UnptxLine::Empty,
    RCurl => UnptxLine::Empty,
    directive[d] => UnptxLine::Directive(d),
    directives[dirs] Ident(id) LParen RParen => {
      // TODO
      UnptxLine::KernelDirective
    }
    inst[i] => UnptxLine::Inst(i),
  }
  directive: Directive {
    DotVersion Int2Lit(major, minor) => {
      let v = match (major, minor) {
        (3, 2) => Version::Ptx_3_2,
        _ => panic!(),
      };
      Directive::Version(v)
    }
    DotTarget Ident(target_arch) => {
      let t = match &target_arch as &str {
        "sm_35" => Target::Sm_3_5,
        _ => panic!(),
      };
      Directive::Target(t)
    }
    DotAddressSize IntLit(bits) => {
      let sz = match bits {
        64 => AddressSize::_64,
        _ => panic!(),
      };
      Directive::AddressSize(sz)
    }
    DotVisible => Directive::Visible,
    DotEntry => Directive::Entry,
  }
  directives: Vec<Directive> {
    directive[d] => {
      vec![d]
    }
    directives[mut dirs] directive[d] => {
      dirs.push(d);
      dirs
    }
  }
  inst: Inst {
    Ret Semi => Inst::Ret,
    BarSync IntLit(_) Semi => Inst::BarSync,
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
      Err(e) => panic!("unptx: syntax error: {:?}", e),
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
