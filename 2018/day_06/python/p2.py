#!/usr/bin/env python3

import re

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

def get_lines(name=filename):
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

last_region_size = -1
valid_region_size = 0

max_valid_dist = 10000

def coord_is_valid(coord):
    total = 0
    for other_coord in coords:
        total += manhattan(coord, other_coord)
    return total < max_valid_dist

for x in range(0, width + 1):
    for y in range(0, height + 1):
        if coord_is_valid((x, y)):
            valid_region_size += 1

xx = width + 1
yy = height + 1
while last_region_size != valid_region_size:
    last_region_size = valid_region_size
    for y in range(yy + 1):
        if coord_is_valid((xx, y)):
            valid_region_size += 1
    for x in range(xx + 1):
        if coord_is_valid((x, yy)):
            valid_region_size += 1

print('The size of the region with total distance < 10,000 from all coords is', valid_region_size)
