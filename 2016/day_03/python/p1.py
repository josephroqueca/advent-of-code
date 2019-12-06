#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

possibilities = 0
for line in PUZZLE_INPUT:
    sides = line.split()
    possible = int(sides[0]) + int(sides[1]) > int(sides[2])
    possible = int(sides[1]) + int(sides[2]) > int(sides[0]) and possible
    possible = int(sides[0]) + int(sides[2]) > int(sides[1]) and possible
    if possible:
        possibilities += 1

print('There are', possibilities, 'possible triangles.')
