#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()

# Initialize to floor 0, start at first position
current_floor = 0
position = 1

# For each character in input
for c in PUZZLE_INPUT:
    # Move up or down one floor based on input
    if c == '(':
        current_floor += 1
    elif c == ')':
        current_floor -= 1

    # When the basement is first reached, print out solution and exit
    if current_floor == -1:
        print("He's made it to the basement at position", position)
        break
    position += 1
