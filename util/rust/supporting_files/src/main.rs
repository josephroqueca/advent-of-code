use std::env;
use std::fs;
use std::path::Path;

/* Part 1
================================================= */

fn part_one(input: String) {
    println!("{}", input);
}

/* Part 2
================================================= */

fn part_two(input: String) {
  println!("{}", input);
}

/* Main + Input
================================================= */

fn main() {
    let args: Vec<String> = env::args().collect();
    let part = &args[1];
    let input = get_input();
    match part.as_str() {
        "1" => part_one(input),
        "2" => part_two(input),
        _ => println!("Only parts 1 and 2 exist...")
    }
}

fn get_test_input() -> String {
    let input_file = Path::new("../test.txt");

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    String::new()
}

fn get_input() -> String {
    let input_file = Path::new("../input.txt");

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    String::new()
}
