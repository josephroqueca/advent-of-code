use std::env;
use std::fs;
use std::path::Path;
use std::collections::HashMap;

/* Part 1
================================================= */

fn part_one(input: String) {
    let double_characters = input.lines()
        .filter(|id| {
            let mut character_count = HashMap::new();
            id.chars().for_each(|c| {
                if character_count.contains_key(&c) {
                    character_count.insert(c, character_count.get(&c).unwrap() + 1);
                } else {
                    character_count.insert(c, 1);
                }
            });

            return character_count.values().filter(|v| **v == 2).count() > 0;
        }).count();

    let triple_characters = input.lines()
        .filter(|id| {
            let mut character_count = HashMap::new();
            id.chars().for_each(|c| {
                if character_count.contains_key(&c) {
                    character_count.insert(c, character_count.get(&c).unwrap() + 1);
                } else {
                    character_count.insert(c, 1);
                }
            });

            return character_count.values().filter(|v| **v == 3).count() > 0;
        }).count();

    let checksum = double_characters * triple_characters;

    println!("The checksum is {}", checksum);
}

/* Part 2
================================================= */

fn part_two(input: String) {
    let mut match_found = false;

    input.lines().for_each(|first| {
        if match_found {
            return;
        }

        input.lines().for_each(|second| {
            let mut different_letter = 0;
            let differing_count = first.chars()
                .enumerate()
                .filter(|(i, x)| second.chars().nth(*i).unwrap() != *x)
                .map(|(i, _)| different_letter = i)
                .count();

            if differing_count == 1 {
                match_found = true;
                println!("The common letters are {}{}", &first[0..different_letter], &first[different_letter + 1..]);
                return;
            }
        });
    });
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

fn get_input() -> String {
    let input_file = Path::new("../input.txt");

    if input_file.exists() {
        return fs::read_to_string(input_file).unwrap();
    }

    String::new()
}
