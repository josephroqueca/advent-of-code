/* Part 1
================================================= */

fn layer_bounds(index: usize, size: usize) -> std::ops::Range<usize> {
    (index * size)..((index + 1) * size)
}

fn count_digits(layer: &str, digit: u32) -> u32 {
    layer
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .filter(|&c| c == digit)
        .count() as u32
}

fn part_one(input: String) {
    let (width, height) = (25, 6);
    let layer_size = width * height;

    let mut fewest_zeroes: Option<(u32, u32)> = None;

    for layer_index in 0..(input.len() as usize / layer_size) {
        let zeroes = count_digits(&input[layer_bounds(layer_index, layer_size)], 0);

        match fewest_zeroes {
            None => fewest_zeroes = Some((zeroes, layer_index as u32)),
            Some(ref i) => {
                if zeroes < i.0 {
                    fewest_zeroes = Some((zeroes, layer_index as u32))
                }
            }
        }
    }

    let layer = fewest_zeroes.unwrap().1;
    let one_digits = count_digits(&input[layer_bounds(layer as usize, layer_size)], 1);
    let two_digits = count_digits(&input[layer_bounds(layer as usize, layer_size)], 2);

    println!("The checksum is {}", one_digits * two_digits);
}

/* Part 2
================================================= */
use std::iter::repeat;

fn part_two(input: String) {
    let (width, height) = (25, 6);
    let layer_size = width * height;

    let mut image: Vec<Vec<u32>> = repeat(repeat(2).take(width).collect())
        .take(height)
        .collect();

    for layer_index in 0..(input.len() as usize / layer_size) {
        input[layer_bounds(layer_index, layer_size)]
            .chars()
            .enumerate()
            .for_each(|(index, value)| {
                let y = index / width;
                let x = index % width;

                if image[y][x] == 2 {
                    image[y][x] = value.to_digit(10).unwrap();
                }
            });
    }

    image.iter().for_each(|layer| println!("{:?}", layer));
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
