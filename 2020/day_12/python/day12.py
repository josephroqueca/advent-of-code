#!/usr/bin/env python3

import aoc
from aoc import Position, Direction

data = aoc.load(year=2020, day=12)

# Part 1

def turn_ship(dir, ins, val):
    directions = [d.name for d in Direction]
    directions = list(reversed(directions)) if ins == 'L' else directions
    rotations = directions.index(dir)
    return (directions[rotations:] + directions[:rotations])[val // 90]

def move_ship(ship, offset, val):
    return Position(ship.x + offset.x * val, ship.y + offset.y * val)

def command_ship(ship, dir, ins, val):
    if ins in ['L', 'R']:
        return ship, turn_ship(dir, ins, val)
    elif ins == 'F':
        return move_ship(ship, Direction[dir].position, val), dir
    else:
        return move_ship(ship, Direction[ins].position, val), dir

ship, direction = Position(0, 0), 'E'
for instruction in data.parse_lines(r'(\w)(\d+)'):
    ship, direction = command_ship(ship, direction, instruction[0], int(instruction[1]))

p1_solution = abs(ship.x) + abs(ship.y)
print(p1_solution)

# Part 2

def rotate_waypoint(waypoint, ins, val):
    while val >= 90:
        if ins == 'R': waypoint = Position(-waypoint.y, waypoint.x)
        if ins == 'L': waypoint = Position(waypoint.y, -waypoint.x)
        val -= 90
    return waypoint

def command_waypoint(ship, waypoint, ins, val):
    if ins in ['L', 'R']:
        return ship, rotate_waypoint(waypoint, ins, val)
    elif ins == 'F':
        return move_ship(ship, waypoint, val), waypoint
    else:
        return ship, move_ship(waypoint, Direction[ins].position, val)


ship, waypoint = Position(0, 0), Position(10, -1)
for instruction in data.parse_lines(r'(\w)(\d+)'):
    ship, waypoint =  command_waypoint(ship, waypoint, instruction[0], int(instruction[1]))

p2_solution = abs(ship.x) + abs(ship.y)
print(p2_solution)
