fn get_range(input: String) -> std::ops::Range<i32> {
    let range = input.split(", ").collect::<Vec<_>>();
    return std::ops::Range {
        start: range[0].parse::<i32>().unwrap(),
        end: range[1].parse::<i32>().unwrap(),
    };
}

/* Part 1
================================================= */

fn has_pair(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 == previous_digit {
            return true;
        }
        previous_digit = value % 10;
    }
    false
}

fn is_incrementing(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 > previous_digit {
            return false;
        }
        previous_digit = value % 10;
    }
    true
}

fn part_one(input: String) {
    let passwords = get_range(input)
        .into_iter()
        .filter(|&candidate| has_pair(candidate) && is_incrementing(candidate))
        .count();
    println!("{} passwords meet the criteria", passwords);
}

/* Part 2
================================================= */

fn has_isolated_pair(candidate: i32) -> bool {
    let mut previous_digit = candidate % 10;
    let mut group_size = 1;
    let mut value = candidate;
    while value >= 10 {
        value = value / 10;
        if value % 10 == previous_digit {
            group_size += 1;
        } else {
            if group_size == 2 {
                return true;
            } else {
                group_size = 1;
            }
        }
        previous_digit = value % 10;
    }
    group_size == 2
}

fn part_two(input: String) {
    let passwords = get_range(input)
        .into_iter()
        .filter(|&candidate| has_isolated_pair(candidate) && is_incrementing(candidate))
        .count();
    println!("{} passwords meet the criteria", passwords);
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(
        &part_one,
        &part_two,
        AOCParams::new(false, Some("256310, 732736".to_string())),
    );
}
