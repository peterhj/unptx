#![feature(proc_macro_hygiene)]

extern crate plex;

use crate::{UnptxToken::*};

use plex::{lexer, parser};

use std::io::{BufRead, Read};

#[allow(non_camel_case_types)]
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
  DotReg,
  DotB32,
  DotB64,
  Colon,
  Semi,
  Comma,
  LParen,
  RParen,
  LBrack,
  RBrack,
  LCurl,
  RCurl,
  // registers
  Tid_X,
  Tid_Y,
  Tid_Z,
  Ntid_X,
  Ntid_Y,
  Ntid_Z,
  Ctaid_X,
  Ctaid_Y,
  Ctaid_Z,
  Nctaid_X,
  Nctaid_Y,
  Nctaid_Z,
  R1,
  Rd1,
  // opcodes
  Trap,
  Ret,
  Bar_Sync,
  Mov_U32,
  Mov_U64,
  St_U32,
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
  r"\.reg" => UnptxToken::DotReg,
  r"\.b32" => UnptxToken::DotB32,
  r"\.b64" => UnptxToken::DotB64,
  r":" => UnptxToken::Colon,
  r";" => UnptxToken::Semi,
  r"," => UnptxToken::Comma,
  r"\(" => UnptxToken::LParen,
  r"\)" => UnptxToken::RParen,
  r"\[" => UnptxToken::LBrack,
  r"\]" => UnptxToken::RBrack,
  r"{" => UnptxToken::LCurl,
  r"}" => UnptxToken::RCurl,
  r"%tid.x" => UnptxToken::Tid_X,
  r"%tid.y" => UnptxToken::Tid_Y,
  r"%tid.z" => UnptxToken::Tid_Z,
  r"%ntid.x" => UnptxToken::Ntid_X,
  r"%ntid.y" => UnptxToken::Ntid_Y,
  r"%ntid.z" => UnptxToken::Ntid_Z,
  r"%ctaid.x" => UnptxToken::Ctaid_X,
  r"%ctaid.y" => UnptxToken::Ctaid_Y,
  r"%ctaid.z" => UnptxToken::Ctaid_Z,
  r"%nctaid.x" => UnptxToken::Nctaid_X,
  r"%nctaid.y" => UnptxToken::Nctaid_Y,
  r"%nctaid.z" => UnptxToken::Nctaid_Z,
  r"%r1" => UnptxToken::R1,
  r"%rd1" => UnptxToken::Rd1,
  r"trap" => UnptxToken::Trap,
  r"ret" => UnptxToken::Ret,
  r"bar\.sync" => UnptxToken::Bar_Sync,
  r"mov\.u32" => UnptxToken::Mov_U32,
  r"mov\.u64" => UnptxToken::Mov_U64,
  r"st\.u32" => UnptxToken::St_U32,
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
pub enum KernelDirective {
  Visible,
  Entry,
}

#[derive(Debug)]
pub enum OtherDirective {
  Func,
  Reg,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Reg {
  Tid_X,
  Tid_Y,
  Tid_Z,
  Ntid_X,
  Ntid_Y,
  Ntid_Z,
  Ctaid_X,
  Ctaid_Y,
  Ctaid_Z,
  Nctaid_X,
  Nctaid_Y,
  Nctaid_Z,
  R1,
  Rd1,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum Inst {
  Group(Vec<Inst>),
  Trap,
  Ret,
  Bar_Sync_0,
  Mov_U32((), ()),
  Mov_U64((), ()),
  St_U32((), ()),
}

pub fn flatten_insts(insts: Vec<Inst>) -> Vec<Inst> {
  let mut flat_insts = Vec::new();
  _flatten_insts(&mut flat_insts, insts);
  flat_insts
}

fn _flatten_insts(flat_insts: &mut Vec<Inst>, insts: Vec<Inst>) {
  for inst in insts.into_iter() {
    match inst {
      Inst::Group(group_insts) => _flatten_insts(flat_insts, group_insts),
      _ => flat_insts.push(inst),
    }
  }
}

#[derive(Debug)]
pub enum UnptxTree {
  Empty,
  ModuleDirective(ModuleDirective),
  KernelDirective,
  FunctionDirective,
  Reg(Reg),
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
    kernel_directives[dirs] Ident(id) LParen RParen => {
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
  kernel_directives: Vec<KernelDirective> {
    => vec![],
    kernel_directives[mut dirs] kernel_directive[d] => {
      dirs.push(d);
      dirs
    }
  }
  kernel_directive: KernelDirective {
    DotVisible => KernelDirective::Visible,
    DotEntry => KernelDirective::Entry,
    //DotReg => Directive::Reg,
    //DotB32 => _,
    //DotB64 => _,
  }
  reg: Reg {
    Tid_X => Reg::Tid_X,
    Tid_Y => Reg::Tid_Y,
    Tid_Z => Reg::Tid_Z,
    Ntid_X => Reg::Ntid_X,
    Ntid_Y => Reg::Ntid_Y,
    Ntid_Z => Reg::Ntid_Z,
    Ctaid_X => Reg::Ctaid_X,
    Ctaid_Y => Reg::Ctaid_Y,
    Ctaid_Z => Reg::Ctaid_Z,
    Nctaid_X => Reg::Nctaid_X,
    Nctaid_Y => Reg::Nctaid_Y,
    Nctaid_Z => Reg::Nctaid_Z,
    R1 => Reg::R1,
    Rd1 => Reg::Rd1,
  }
  insts: Vec<Inst> {
    => vec![],
    insts[mut ii] inst[i] => {
      ii.push(i);
      ii
    }
  }
  inst: Inst {
    LCurl insts[ii] RCurl => Inst::Group(ii),
    Trap Semi => Inst::Trap,
    Ret Semi => Inst::Ret,
    Bar_Sync IntLit(0) Semi => Inst::Bar_Sync_0,
    Bar_Sync IntLit(i) Semi => panic!("unsupported bar.sync int argument: {:?}", i),
    //Mov_U32 reg[dst] Comma reg[src] Semi => Inst::Mov_U32(dst, src),
    //Mov_U64 reg[dst] Comma reg[src] Semi => Inst::Mov_U64(dst, src),
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
