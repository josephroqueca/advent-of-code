#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

# Keypad layout
layout = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

# Initial position (number 5)
start = (1, 1)
code = []

for line in PUZZLE_INPUT:
    for char in line:
        if char == 'L':
            start = (start[0], max(start[1] - 1, 0))
        if char == 'U':
            start = (max(start[0] - 1, 0), start[1])
        if char == 'R':
            start = (start[0], min(start[1] + 1, 2))
        if char == 'D':
            start = (min(start[0] + 1, 2), start[1])

    code.append(layout[start[0]][start[1]])

print('The code for the bathroom is', code)
