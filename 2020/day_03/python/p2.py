#!/usr/bin/env python3

import os
import math
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


slopes = [
    Position(1, 1),
    Position(3, 1),
    Position(5, 1),
    Position(7, 1),
    Position(1, 2),
]


def count_trees(slope):
    position = Position(0, 0)
    trees = 0
    while position.y < len(hillside):
        if hillside[position.y][position.x % hillside_width]: trees += 1
        position = Position(position.x + slope.x, position.y + slope.y)
    return trees


trees = math.prod([count_trees(slope) for slope in slopes])

print("The product of all the encountered trees is {}".format(trees))
