#!/usr/bin/env python3

import os
from collections import namedtuple

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution

Position = namedtuple('Position', ['x', 'y'])

hillside = []
for line in get_lines():
    hillside.append([])
    for space in line:
        hillside[-1].append(space == '#')
hillside_width = len(hillside[0])

position = Position(0, 0)
slope = Position(3, 1)
trees = 0

while position.y < len(hillside):
    if hillside[position.y][position.x % hillside_width]: trees += 1
    position = Position(position.x + slope.x, position.y + slope.y)

print("You would encounter {} trees".format(trees))
