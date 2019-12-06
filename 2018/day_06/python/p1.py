#!/usr/bin/env python3

import re

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)


def get_lines(name=FILENAME):
    with open(name) as input_file:
        return input_file.readlines()


def get_nums_by_line():
    return [tuple([int(match) for match in re.findall(r'-?\d+', line)]) for line in get_lines()]


def manhattan(coord, coord2):
    return abs(coord[0] - coord2[0]) + abs(coord[1] - coord2[1])


coords = get_nums_by_line()

width = max([x for x, _ in coords])
height = max([y for _, y in coords])
left = min([x for x, _ in coords])
top = min([y for _, y in coords])

region_sizes = dict(zip(coords, [0] * len(coords)))

for region_center in region_sizes:
    x, y = region_center
    if x == left or x == width or y == top or y == height:
        region_sizes[region_center] = -1


def find_closest(coord):
    min_dist = max(width, height)
    current_closest = None
    for center in region_sizes:
        dist = manhattan(coord, center)
        if dist < min_dist:
            current_closest = center
            min_dist = dist
        elif dist == min_dist:
            current_closest = None
    return current_closest


for x in range(left, width + 1):
    for y in range(top, height + 1):
        closest = find_closest((x, y))
        if closest is not None and region_sizes[closest] >= 0:
            region_sizes[closest] += 1

print('The largest region is', max(region_sizes.values()))
