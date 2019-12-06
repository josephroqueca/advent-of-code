use std::collections::{HashMap, HashSet};

/* Common
================================================= */

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn manhattan_distance(&self, other: Point) -> i32 {
        return (self.x - other.x).abs() + (self.y - other.y).abs();
    }
}

/* Part 1
================================================= */

fn explore_wire(input: String) -> HashSet<Point> {
    let mut wire: HashSet<Point> = HashSet::new();
    input
        .split(",")
        .fold(Point { x: 0, y: 0 }, |acc, instruction| {
            let distance = instruction[1..].parse::<i32>().unwrap();
            let modifier = match &instruction[0..1] {
                "R" => Point { x: 1, y: 0 },
                "L" => Point { x: -1, y: 0 },
                "U" => Point { x: 0, y: 1 },
                "D" => Point { x: 0, y: -1 },
                _ => unreachable!(),
            };

            return (0..distance).fold(acc, |acc, _| {
                let next = Point {
                    x: acc.x + modifier.x,
                    y: acc.y + modifier.y,
                };
                wire.insert(next);
                next
            });
        });
    wire
}

fn part_one(input: String) {
    let origin = Point { x: 0, y: 0 };

    let wires = input
        .split_whitespace()
        .map(|x| explore_wire(x.to_string()))
        .collect::<Vec<_>>();

    let primary_intersection = wires[0]
        .intersection(&wires[1])
        .fold(origin, |closest, next| match closest {
            Point { x: 0, y: 0 } => *next,
            _ => {
                if origin.manhattan_distance(closest) <= origin.manhattan_distance(*next) {
                    closest
                } else {
                    *next
                }
            }
        });

    println!(
        "The manhattan distance is {}",
        origin.manhattan_distance(primary_intersection)
    );
}

/* Part 2
================================================= */

fn measure_wire(input: String) -> HashMap<Point, i32> {
    let mut wire: HashMap<Point, i32> = HashMap::new();
    input
        .split(",")
        .fold((Point { x: 0, y: 0 }, 0), |acc, instruction| {
            let distance = instruction[1..].parse::<i32>().unwrap();
            let modifier = match &instruction[0..1] {
                "R" => Point { x: 1, y: 0 },
                "L" => Point { x: -1, y: 0 },
                "U" => Point { x: 0, y: 1 },
                "D" => Point { x: 0, y: -1 },
                _ => unreachable!(),
            };

            return (0..distance).fold(acc, |acc, _| {
                let next = Point {
                    x: acc.0.x + modifier.x,
                    y: acc.0.y + modifier.y,
                };
                if wire.contains_key(&next) == false {
                    wire.insert(next, acc.1 + 1);
                }
                (next, acc.1 + 1)
            });
        });
    wire
}

fn part_two(input: String) {
    let wires = input
        .split_whitespace()
        .map(|x| measure_wire(x.to_string()))
        .collect::<Vec<_>>();

    let right_wire_points: HashSet<Point> = wires[0].keys().cloned().collect();
    let left_wire_points: HashSet<Point> = wires[1].keys().cloned().collect();

    let min_distance = right_wire_points.intersection(&left_wire_points).fold(
        i32::max_value(),
        |min_dist, &point| {
            let distance = wires[0].get(&point).unwrap() + wires[1].get(&point).unwrap();
            if distance < min_dist {
                distance
            } else {
                min_dist
            }
        },
    );

    println!(
        "The fewest steps to reach an intersection is {}",
        min_distance
    );
}

/* Main + Input
================================================= */

use aoc_util::{aoc, AOCParams};

fn main() {
    aoc(&part_one, &part_two, AOCParams::new(false, None));
}
