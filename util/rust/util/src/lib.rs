use std::env;
use std::fs;
use std::path::Path;

pub fn aoc(p1: &dyn Fn(String) -> (), p2: &dyn Fn(String) -> (), testing: bool) {
    let args: Vec<String> = env::args().collect();
    let part = &args[1];
    let input = if testing {
        get_test_input()
    } else {
        get_input()
    };

    match part.as_str() {
        "1" => p1(input),
        "2" => p2(input),
        _ => println!("Only parts 1 and 2 exist..."),
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
