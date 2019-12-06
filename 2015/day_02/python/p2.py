#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Initialize to 0 feet of ribbon
total_length = 0

# For each line in the input
for line in PUZZLE_INPUT:

    # Get the integers in each line
    sides = re.split(r'\D', line)
    sides = [int(i) for i in sides[:3]]

    # Start by calculating the volume of the gift
    total_length += sides[0] * sides[1] * sides[2]

    # Find 2 smallest sides and add their perimeter to the total
    if sides[0] < sides[1]:
        if sides[2] < sides[1]:
            total_length += 2 * (sides[0] + sides[2])
        else:
            total_length += 2 * (sides[0] + sides[1])
    else:
        if sides[2] < sides[0]:
            total_length += 2 * (sides[1] + sides[2])
        else:
            total_length += 2 * (sides[0] + sides[1])

# Print the total length of ribbo needed
print('The elves will need', total_length, 'feet of ribbon!')
