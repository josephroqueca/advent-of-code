use opcode_computer::Program;

/* Part 1
================================================= */

fn part_one(input: String) {
    println!(
        "The BOOST program outputs {}",
        Program::from_str(&input)
            .push(1)
            .run()
            .output()
            .pop()
            .unwrap()
    );
}

/* Part 2
================================================= */

fn part_two(input: String) {
    println!(
        "The BOOST program outputs coordinates ({})",
        Program::from_str(&input)
            .push(2)
            .run()
            .output()
            .pop()
            .unwrap()
    );
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
