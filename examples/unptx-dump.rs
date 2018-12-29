extern crate unptx;

use std::env;
use std::fs::{File};
use std::io::{BufReader};
use std::path::{PathBuf};

fn main() {
  let args: Vec<_> = env::args().collect();
  let file = File::open(PathBuf::from(&args[1])).unwrap();
  let reader = BufReader::new(file);
  /*for line in unptx::UnptxLines::from_reader(reader) {
    if !line.is_empty() {
      println!("{:?}", line);
    }
  }*/
  let ptx_module = unptx::UnptxModule::from_reader(reader).unwrap();
  println!("{:?}", ptx_module);
}
