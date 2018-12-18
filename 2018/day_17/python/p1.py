#!/usr/bin/env python3

import re
import json
import itertools
import hashlib

test_input = True

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'test' if test_input else 'input')

def get_file():
    with open(filename) as f:
        return f.read()

def get_lines():
    with open(filename) as f:
        return [line.strip() for line in f.readlines()]

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]

clay = set()
left = 500
right = 500
min_height = 1
max_height = 1

for line in get_lines():
    values = get_numbers_from_line(line)
    if line[0] == 'x':
        x = values[0]
        left = min(left, x)
        right = max(right, x)
        min_height = min(min_height, values[1])
        max_height = max(min_height, values[2])
        for y in range(values[1], values[2] + 1):
            clay.add((x, y))
    else:
        y = values[0]
        left = min(left, values[1])
        right = max(right, values[2])
        min_height = min(min_height, y)
        max_height = max(max_height, y)
        for x in range(values[1], values[2] + 1):
            clay.add((x, y))

class Water(object):
    def __init__(self, tile, is_source, moved_left, moved_right):
        self.tile = tile
        self.is_source = is_source
        self.moved_left = moved_left
        self.moved_right = moved_right
    def __eq__(self, other):
        return self.tile == other.tile and self.is_source == other.is_source and self.moved_left == other.moved_left and self.moved_right == other.moved_right
    def __hash__(self):
        return hash(self.tile)
    def __repr__(self):
        return "({}, {}, {})".format(self.tile, self.moved_left, self.moved_right)

starting_tile = (500, 0)
starting_water = Water(starting_tile, True, False, False)
left_settled = set()
right_settled = set()
touched = set()
moving_water = [starting_water]

def print_status(current_water):
    print('==========')
    for y in range(min_height, max_height + 1):
        row = ""
        for x in range(left, right + 1):
            tile = (x, y)
            if tile == current_water.tile:
                row += '+'
            elif tile in clay:
                row += '#'
            elif is_settled(tile):
                row += '~'
            elif tile in touched:
                row += '|'
            else:
                row += '.'
        print(row)
    print('left', left_settled)
    print('right', right_settled)
    print('left, not right:', left_settled.difference(right_settled))
    print('right, not left:', right_settled.difference(left_settled))
    print('moving', moving_water)
    print('==========')

def is_settled(tile):
    return tile in left_settled and tile in right_settled

last_infinity_reached = None
infinity_reached = set()
iterations = 0

while last_infinity_reached is None or last_infinity_reached != infinity_reached:
    last_infinity_reached = set(infinity_reached)
    infinity_reached = set()

    left_touched_this_iteration = set()
    right_touched_this_iteration = set()
    left_settled.intersection_update(right_settled)
    right_settled.intersection_update(left_settled)

    while moving_water:
        water = moving_water.pop()
        tile = water.tile

        if tile in clay or is_settled(tile):
            continue

        if tile[1] >= min_height and tile[1] <= max_height:
            touched.add(tile)

        if water.moved_left:
            left_touched_this_iteration.add(tile)
        if water.moved_right:
            right_touched_this_iteration.add(tile)

        iterations += 1
        if iterations % 100000 == 0:
            print_status(water)
        print_status(water)
        input()

        left_tile = (tile[0] - 1, tile[1])
        right_tile = (tile[0] + 1, tile[1])
        below_tile = (tile[0], tile[1] + 1)

        if water.is_source:
            left_water = Water(left_tile, False, True, False)
            right_water = Water(right_tile, False, False, True)
            below_water = Water(below_tile, True, False, False)
            if (is_settled(left_tile) or left_tile in clay) and (is_settled(right_tile) or right_tile in clay):
                left_settled.add(tile)
                right_settled.add(tile)
                continue
        else:
            left_water = Water(left_tile, False, water.moved_left, water.moved_right)
            right_water = Water(right_tile, False, water.moved_left, water.moved_right)
            below_water = Water(below_tile, True, False, False)

        if below_tile in clay or is_settled(below_tile):
            if left_tile in clay or left_tile in left_settled:
                if not water.is_source: left_settled.add(tile)
                if right_tile not in left_settled:
                    moving_water.append(right_water)
            elif left_tile not in left_touched_this_iteration:
                moving_water.append(left_water)

            if right_tile in clay or right_tile in right_settled:
                if not water.is_source: right_settled.add(tile)
                if left_tile not in right_settled:
                    moving_water.append(left_water)
            elif right_tile not in right_touched_this_iteration:
                moving_water.append(right_water)
        else:
            if below_tile[1] > max_height:
                infinity_reached.add(tile)
            else:
                moving_water.append(below_water)

    if not infinity_reached:
        last_infinity_reached = None
    moving_water.append(starting_water)

print(len(touched))
