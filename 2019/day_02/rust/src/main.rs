use std::env;
use std::fs;
use std::path::Path;

/* Part 1
================================================= */

fn part_one(input: String) {
    let mut intcode = get_numbers(&input).collect::<Vec<_>>();
    intcode[1] = 12;
    intcode[2] = 2;

    let mut i = 0;

    while intcode[i] != 99 {
        let register = intcode[i + 3];

        match intcode[i] {
            1 => intcode[register] = intcode[intcode[i + 1]] + intcode[intcode[i + 2]],
            2 => intcode[register] = intcode[intcode[i + 1]] * intcode[intcode[i + 2]],
            _ => unreachable!()
        }

        i += 4;
    }

    println!("The value at position 0 is {}", intcode[0]);
}

/* Part 2
================================================= */

fn part_two(input: String) {
    for noun in 0..99 {
        for verb in 0..99 {
            let mut intcode = get_numbers(&input).collect::<Vec<_>>();
            intcode[1] = noun;
            intcode[2] = verb;

            let mut i = 0;

            while intcode[i] != 99 {
                let register = intcode[i + 3];

                match intcode[i] {
                    1 => intcode[register] = intcode[intcode[i + 1]] + intcode[intcode[i + 2]],
                    2 => intcode[register] = intcode[intcode[i + 1]] * intcode[intcode[i + 2]],
                    _ => unreachable!()
                }

                i += 4;
            }

            if intcode[0] == 19690720 {
                println!("The noun is {} and the verb is {}", noun, verb);
            }
        }
    }
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

    return String::new();
}

fn get_input() -> String {
    let input_file = Path::new("../input.txt");

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    return String::new();
}

fn get_numbers<'a>(text: &'a str) -> impl Iterator<Item = usize> + 'a {
    text.split(",").map(|x| x.parse::<usize>().unwrap())
}
