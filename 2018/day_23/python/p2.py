#!/usr/bin/env python3

test_input = False

import re
import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'test' if test_input else 'input')

def get_lines():
    with open(filename) as f:
        return [line.strip() for line in f.readlines()]

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

def manhattan(first, second):
    return abs(first[0][0] - second[0][0]) + abs(first[0][1] - second[0][1]) + abs(first[0][2] - second[0][2])

def points_in_range(bot):
    return [(x, y, z) for x in range(bot[0][0] - bot[1], bot[0][0] + bot[1])
        for y in range(bot[0][1] - bot[1], bot[0][1] + bot[1])
        for z in range(bot[0][2] - bot[1], bot[0][2] + bot[1])]

nanobots = [((vals[0], vals[1], vals[2]), vals[3]) for vals in get_numbers_by_line()]
nanobots.sort(key=lambda tup: tup[1])

most_connected_bot = (0, None)

for bot in nanobots:
    connections = sum([1 if manhattan(bot, neighbor) <= bot[1] else 0 for neighbor in nanobots])
    if connections > most_connected_bot[0]:
        most_connected_bot = (connections, bot)

print(most_connected_bot)
print(len(points_in_range(most_connected_bot[1])))



# # for i in range(sample_size):
    


# in_range = sum([1 if manhattan(x, strongest) <= strongest[1] else 0 for x in nanobots])

# print('There are {} nanobots in range of the strongest.'.format(in_range))
