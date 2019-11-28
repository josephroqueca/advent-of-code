use std::env;

fn part_one() {
  println!("Hello, world! 1");
}

fn part_two() {
  println!("Hello, world! 2")
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let part = &args[1];

  match part.as_str() {
    "1" => part_one(),
    "2" => part_two(),
    _ => println!("Only parts 1 and 2 exist...")
  }
}
