#!/usr/bin/env python3

import re
import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'input')

def get_lines():
    with open(filename) as f:
        return [line.strip() for line in f.readlines()]

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

nanobots = [((vals[0], vals[1], vals[2]), vals[3]) for vals in get_numbers_by_line()]
nanobots.sort(key=lambda tup: tup[1], reverse=True)
strongest = nanobots[0]

def manhattan(first, second):
    return abs(first[0][0] - second[0][0]) + abs(first[0][1] - second[0][1]) + abs(first[0][2] - second[0][2])

in_range = sum([1 if manhattan(x, strongest) <= strongest[1] else 0 for x in nanobots])

print('There are {} nanobots in range of the strongest.'.format(in_range))
