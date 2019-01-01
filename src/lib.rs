#![feature(proc_macro_hygiene)]

extern crate plex;

use crate::{UnptxToken::*};

use plex::{lexer, parser};

use std::io::{BufRead, Read};

#[derive(Debug)]
pub enum UnptxToken {
  Whitespace,
  LineComment,
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
  r"//[^\n]*" => UnptxToken::LineComment,
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

pub struct UnptxLexer<'s> {
  buf:  &'s str,
  eol:  bool,
}

impl<'s> UnptxLexer<'s> {
  pub fn new(buf: &'s str) -> UnptxLexer<'s> {
    UnptxLexer{
      buf,
      eol:  false,
    }
  }
}

impl<'s> Iterator for UnptxLexer<'s> {
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
            UnptxToken::Whitespace |
            UnptxToken::LineComment => {
              continue;
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

// NB: These PTX versions should correspond exactly to the ones in the
// LLVM NVPTX backend (see: lib/Target/NVPTX/NVPTX.td).
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Version {
  Ptx_3_2,
  Ptx_4_0,
  Ptx_4_1,
  Ptx_4_2,
  Ptx_4_3,
  Ptx_5_0,
  Ptx_6_0,
  Ptx_6_1,
  Ptx_6_3,
}

// NB: These target archs should correspond exactly to the ones in the
// LLVM NVPTX backend (see: lib/Target/NVPTX/NVPTX.td).
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Target {
  Sm_2_0,
  Sm_2_1,
  Sm_3_0,
  Sm_3_2,
  Sm_3_5,
  Sm_3_7,
  Sm_5_0,
  Sm_5_2,
  Sm_5_3,
  Sm_6_0,
  Sm_6_1,
  Sm_6_2,
  Sm_7_0,
  Sm_7_2,
  Sm_7_5,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AddressSize {
  _32,
  _64,
}

#[derive(Debug)]
pub enum ModuleDirective {
  Version(Version),
  Target(Target),
  AddressSize(AddressSize),
}

#[derive(Debug)]
pub enum Directive {
  Visible,
  Entry,
  Func,
}

#[derive(Debug)]
pub enum Inst {
  //Group(InstGroup),
  Group(Vec<Inst>),
  Ret,
  BarSync,
}

#[derive(Debug)]
pub struct InstGroup(pub Vec<Inst>);

#[derive(Debug)]
pub enum UnptxTree {
  Empty,
  ModuleDirective(ModuleDirective),
  KernelDirective,
  FunctionDirective,
  Inst(Inst),
}

impl UnptxTree {
  pub fn from_reader<R: Read>(mut reader: R) -> Vec<UnptxTree> {
    let mut text = String::new();
    match reader.read_to_string(&mut text) {
      Err(e) => panic!("failed to read text: {:?}", e),
      Ok(_) => {}
    }
    match UnptxTree::parse(UnptxLexer::new(&text)) {
      Err(e) => panic!("parse failure: {:?}", e),
      Ok(trees) => trees,
    }
  }

  pub fn parse<L: Iterator<Item=UnptxToken>>(lexer: L) -> Result<Vec<UnptxTree>, (Option<(UnptxToken, ())>, &'static str)> {
    parse_trees(lexer.map(|tok| (tok, ())))
  }
}

parser! {
  fn parse_trees(UnptxToken, ());
  trees: Vec<UnptxTree> {
    => vec![],
    trees[mut tt] tree[t] => {
      tt.push(t);
      tt
    }
  }
  tree: UnptxTree {
    //=> UnptxTree::Empty,
    module_directive[d] => UnptxTree::ModuleDirective(d),
    directives[dirs] Ident(id) LParen RParen => {
      // TODO
      UnptxTree::KernelDirective
    }
    inst[i] => UnptxTree::Inst(i),
  }
  module_directive: ModuleDirective {
    DotVersion Int2Lit(major, minor) => {
      let v = match (major, minor) {
        (3, 2) => Version::Ptx_3_2,
        (4, 0) => Version::Ptx_4_0,
        (4, 1) => Version::Ptx_4_1,
        (4, 2) => Version::Ptx_4_2,
        (4, 3) => Version::Ptx_4_3,
        (5, 0) => Version::Ptx_5_0,
        (6, 0) => Version::Ptx_6_0,
        (6, 1) => Version::Ptx_6_1,
        (6, 3) => Version::Ptx_6_3,
        _ => panic!(),
      };
      ModuleDirective::Version(v)
    }
    DotTarget Ident(target_arch) => {
      let t = match &target_arch as &str {
        "sm_20" => Target::Sm_2_0,
        "sm_21" => Target::Sm_2_1,
        "sm_30" => Target::Sm_3_0,
        "sm_32" => Target::Sm_3_2,
        "sm_35" => Target::Sm_3_5,
        "sm_37" => Target::Sm_3_7,
        "sm_50" => Target::Sm_5_0,
        "sm_52" => Target::Sm_5_2,
        "sm_53" => Target::Sm_5_3,
        "sm_60" => Target::Sm_6_0,
        "sm_61" => Target::Sm_6_1,
        "sm_62" => Target::Sm_6_2,
        "sm_70" => Target::Sm_7_0,
        "sm_72" => Target::Sm_7_2,
        "sm_75" => Target::Sm_7_5,
        _ => panic!(),
      };
      ModuleDirective::Target(t)
    }
    DotAddressSize IntLit(bits) => {
      let sz = match bits {
        32 => AddressSize::_32,
        64 => AddressSize::_64,
        _ => panic!(),
      };
      ModuleDirective::AddressSize(sz)
    }
  }
  directive: Directive {
    DotVisible => Directive::Visible,
    DotEntry => Directive::Entry,
  }
  directives: Vec<Directive> {
    => vec![],
    directives[mut dirs] directive[d] => {
      dirs.push(d);
      dirs
    }
  }
  inst: Inst {
    LCurl insts[ii] RCurl => Inst::Group(ii),
    Ret Semi => Inst::Ret,
    BarSync IntLit(_) Semi => Inst::BarSync,
  }
  insts: Vec<Inst> {
    => vec![],
    insts[mut ii] inst[i] => {
      ii.push(i);
      ii
    }
  }
}

#[derive(Debug)]
pub enum UnptxLine {
  Empty,
  ModuleDirective(ModuleDirective),
  KernelDirective,
  FunctionDirective,
  Inst(Inst),
}

impl UnptxLine {
  pub fn parse<L: Iterator<Item=UnptxToken>>(line_lexer: L) -> Result<UnptxLine, (Option<(UnptxToken, ())>, &'static str)> {
    parse(line_lexer.map(|tok| (tok, ())))
  }

  pub fn is_empty(&self) -> bool {
    match self {
      &UnptxLine::Empty => true,
      _ => false,
    }
  }
}

parser! {
  fn parse(UnptxToken, ());
  line: UnptxLine {
    => UnptxLine::Empty,
    LCurl => UnptxLine::Empty,
    RCurl => UnptxLine::Empty,
    module_directive[d] => UnptxLine::ModuleDirective(d),
    directives[dirs] Ident(id) LParen RParen => {
      // TODO
      UnptxLine::KernelDirective
    }
    inst[i] => UnptxLine::Inst(i),
  }
  module_directive: ModuleDirective {
    DotVersion Int2Lit(major, minor) => {
      let v = match (major, minor) {
        (3, 2) => Version::Ptx_3_2,
        (4, 0) => Version::Ptx_4_0,
        (4, 1) => Version::Ptx_4_1,
        (4, 2) => Version::Ptx_4_2,
        (4, 3) => Version::Ptx_4_3,
        (5, 0) => Version::Ptx_5_0,
        (6, 0) => Version::Ptx_6_0,
        (6, 1) => Version::Ptx_6_1,
        (6, 3) => Version::Ptx_6_3,
        _ => panic!(),
      };
      ModuleDirective::Version(v)
    }
    DotTarget Ident(target_arch) => {
      let t = match &target_arch as &str {
        "sm_20" => Target::Sm_2_0,
        "sm_21" => Target::Sm_2_1,
        "sm_30" => Target::Sm_3_0,
        "sm_32" => Target::Sm_3_2,
        "sm_35" => Target::Sm_3_5,
        "sm_37" => Target::Sm_3_7,
        "sm_50" => Target::Sm_5_0,
        "sm_52" => Target::Sm_5_2,
        "sm_53" => Target::Sm_5_3,
        "sm_60" => Target::Sm_6_0,
        "sm_61" => Target::Sm_6_1,
        "sm_62" => Target::Sm_6_2,
        "sm_70" => Target::Sm_7_0,
        "sm_72" => Target::Sm_7_2,
        "sm_75" => Target::Sm_7_5,
        _ => panic!(),
      };
      ModuleDirective::Target(t)
    }
    DotAddressSize IntLit(bits) => {
      let sz = match bits {
        32 => AddressSize::_32,
        64 => AddressSize::_64,
        _ => panic!(),
      };
      ModuleDirective::AddressSize(sz)
    }
  }
  directive: Directive {
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

pub struct UnptxLines<R> {
  reader:   R,
}

impl<R> UnptxLines<R> {
  pub fn from_reader(reader: R) -> UnptxLines<R> {
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
    match UnptxLine::parse(UnptxLexer::new(&buf)) {
      Err(e) => panic!("unptx: syntax error: {:?}", e),
      Ok(line) => Some(line),
    }
  }
}

#[derive(Default)]
pub struct UnptxModuleBuilder {
  version:  Option<Version>,
  target:   Option<Target>,
  addrsize: Option<AddressSize>,
  //kernels:      Vec<()>,
  //functions:    Vec<()>,
  _state:   (),
}

impl UnptxModuleBuilder {
  fn maybe_into(self) -> Result<UnptxModule, &'static str> {
    Ok(UnptxModule{
      version:  self.version.ok_or_else(|| "missing .version")?,
      target:   self.target.ok_or_else(|| "missing .target")?,
      addrsize: self.addrsize.ok_or_else(|| "missing .address_size")?,
    })
  }

  pub fn with_lines<I: Iterator<Item=UnptxLine>>(mut self, lines: I) -> Result<UnptxModule, &'static str> {
    for line in lines {
      // TODO
      match (self._state, line) {
        (_, UnptxLine::ModuleDirective(dir)) => {
          match dir {
            ModuleDirective::Version(version) => {
              if self.version.is_some() {
                return Err("duplicate .version");
              }
              self.version = Some(version);
            }
            ModuleDirective::Target(target) => {
              if self.target.is_some() {
                return Err("duplicate .target");
              }
              self.target = Some(target);
            }
            ModuleDirective::AddressSize(addrsize) => {
              if self.addrsize.is_some() {
                return Err("duplicate .address_size");
              }
              self.addrsize = Some(addrsize);
            }
          }
        }
        _ => {}
        //_ => unimplemented!(),
      }
    }
    self.maybe_into()
  }

  pub fn with_trees<T: Iterator<Item=UnptxTree>>(mut self, trees: T) -> Result<UnptxModule, &'static str> {
    for tree in trees {
      // TODO
      match (self._state, tree) {
        (_, UnptxTree::ModuleDirective(dir)) => {
          println!("DEBUG: ptx module builder: got module directive: {:?}", dir);
          match dir {
            ModuleDirective::Version(version) => {
              if self.version.is_some() {
                return Err("duplicate .version");
              }
              self.version = Some(version);
            }
            ModuleDirective::Target(target) => {
              if self.target.is_some() {
                return Err("duplicate .target");
              }
              self.target = Some(target);
            }
            ModuleDirective::AddressSize(addrsize) => {
              if self.addrsize.is_some() {
                return Err("duplicate .address_size");
              }
              self.addrsize = Some(addrsize);
            }
          }
        }
        (_, UnptxTree::Inst(inst)) => {
          println!("DEBUG: ptx module builder: got inst tree: {:?}", inst);
        }
        (_, tree) => {
          println!("DEBUG: ptx module builder: got tree: {:?}", tree);
        }
        //_ => unimplemented!(),
      }
    }
    self.maybe_into()
  }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UnptxModule {
  pub version:  Version,
  pub target:   Target,
  pub addrsize: AddressSize,
}

impl UnptxModule {
  pub fn from_reader<R: BufRead>(reader: R) -> Result<UnptxModule, &'static str> {
    UnptxModuleBuilder::default()
      .with_trees(UnptxTree::from_reader(reader).into_iter())
  }
}
