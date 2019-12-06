#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()

# Initialize to floor 0
current_floor = 0

# For each character in input
for c in PUZZLE_INPUT:

    # Move up or down one floor based on character
    if c == '(':
        current_floor += 1
    elif c == ')':
        current_floor -= 1

# When done, print the current floor
print('Santa is on floor', current_floor)
