#!/usr/bin/env python3

import re
import json
import itertools
import hashlib

test_input = False

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

settled_water = set()

def print_status(current_tile):
    for y in range(min_height, max_height + 1):
        row = ""
        for x in range(left - 1, right + 1):
            tile = (x, y)
            if tile in clay:
                row += '#'
            elif tile == current_tile:
                row += '+'
            elif tile in settled_water:
                row += '~'
            elif tile in touched:
                row += '|'
            else:
                row += '.'
        print(row)
    print('touched', len(touched))
    print('moving', moving_water, current_tile)
    print('====================')

def print_snippet(current_tile):
    min_y = max(min_height, current_tile[1] - 10)
    max_y = min(max_height, current_tile[1] + 10)
    for y in range(min_y, max_y):
        row = ""
        for x in range(left - 1, right + 1):
            tile = (x, y)
            if tile in clay:
                row += '#'
            elif tile == current_tile:
                row += '+'
            elif tile in settled_water:
                row += '~'
            elif tile in touched:
                row += '|'
            else:
                row += '.'
        print(row)
    print('touched', len(touched))
    print('moving', moving_water, current_tile)
    print('====================')

def left_of(tile):
    return (tile[0] - 1, tile[1])
def right_of(tile):
    return (tile[0] + 1, tile[1])
def under(tile):
    return (tile[0], tile[1] + 1)
def above(tile):
    return (tile[0], tile[1] - 1)
def in_range(tile):
    return left <= tile[0] <= right and min_height <= tile[1] <= max_height

def find_right_edge(current_tile):
    right_tile = current_tile
    under_tile = under(right_tile)
    while in_range(right_tile) and right_tile not in clay and (under_tile in clay or under_tile in settled_water):
        right_tile = right_of(right_tile)
        under_tile = under(right_tile)
    if right_tile in clay:
        return left_of(right_tile), True
    else:
        return right_tile, False

def find_left_edge(current_tile):
    left_tile = current_tile
    under_tile = under(left_tile)
    while in_range(left_tile) and left_tile not in clay and (under_tile in clay or under_tile in settled_water):
        left_tile = left_of(left_tile)
        under_tile = under(left_tile)
    if left_tile in clay:
        return right_of(left_tile), True
    else:
        return left_tile, False

starting_tile = (500, 0)
moving_water = []
touched = set()

iterations = 0
spilled_water = set()

while not spilled_water or moving_water:
    water = moving_water.pop() if moving_water else starting_tile

    # print_snippet(water)
    # input()

    left_edge, left_enclosed = find_left_edge(water)
    right_edge, right_enclosed = find_right_edge(water)

    if in_range(water):
        touched.add(water)

    for x in range(left_edge[0], right_edge[0] + 1):
        cell = (x, water[1])
        if left_enclosed and right_enclosed:
            settled_water.add(cell)
        if in_range(cell):
            touched.add(cell)

    under_left = under(left_edge)
    if not left_enclosed:
        if in_range(under_left):
            moving_water.append(under_left)
        else:
            spilled_water.add(under_left)

    under_right = under(right_edge)
    if not right_enclosed:
        if in_range(under_right) and (not moving_water or moving_water[-1] != under_right):
            moving_water.append(under_right)
        else:
            spilled_water.add(under_right)

    if left_enclosed and right_enclosed:
        moving_water.append(above(water))

print_status(water)
print(len(touched))
