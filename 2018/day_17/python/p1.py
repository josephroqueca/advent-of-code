#!/usr/bin/env python3

import re

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]


clay = set()
left = right = 500
min_height = max_height = 1

for l in get_lines():
    values = get_numbers_from_line(l)
    if l[0] == 'x':
        x = values[0]
        left = min(left, x)
        right = max(right, x)
        min_height = min(min_height, values[1])
        max_height = max(max_height, values[2])
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

left = left - 1
right = right + 1


def left_of(tile):
    return (tile[0] - 1, tile[1])


def right_of(tile):
    return (tile[0] + 1, tile[1])


def under(tile):
    return (tile[0], tile[1] + 1)


def above(tile):
    return (tile[0], tile[1] - 1)


def in_range(tile):
    return left <= tile[0] <= right and min_height <= tile[1] < max_height


def find_right_edge(current_tile):
    right_tile = current_tile
    under_tile = under(right_tile)
    while in_range(right_tile) and right_tile not in clay and (under_tile in clay or under_tile in settled_water):
        right_tile = right_of(right_tile)
        under_tile = under(right_tile)
    if right_tile in clay:
        return left_of(right_tile), True
    return right_tile, False


def find_left_edge(current_tile):
    left_tile = current_tile
    under_tile = under(left_tile)
    while in_range(left_tile) and left_tile not in clay and (under_tile in clay or under_tile in settled_water):
        left_tile = left_of(left_tile)
        under_tile = under(left_tile)
    if left_tile in clay:
        return right_of(left_tile), True
    return left_tile, False


starting_tile = (500, 0)
moving_water = []
touched = set()
spilled_water = set()
settled_water = set()

while not spilled_water or moving_water:
    water = moving_water.pop() if moving_water else starting_tile

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

print('The water has touched a total of {} spaces.'.format(len(touched)))
