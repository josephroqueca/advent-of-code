#!/usr/bin/env python3

from collections import namedtuple
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


# Tuple to use as dictionary key
Location = namedtuple('Location', ['x', 'y'])

# Start dictionary with Santa's starting location
presents_delivered = {Location(0, 0): 1}

# Initialize Santa and robot's starting location
current_x = [0, 0]
current_y = [0, 0]
santas_turn = True

# For each character in the input
for c in PUZZLE_INPUT:

    # Swapping turns
    offset = 0
    if not santas_turn:
        offset = 1
    santas_turn = not santas_turn

    # Move current person based on the character
    if c == '<':
        current_x[offset] -= 1
    elif c == '>':
        current_x[offset] += 1
    elif c == '^':
        current_y[offset] += 1
    elif c == 'v':
        current_y[offset] -= 1

    # Check if the location is in the dictionary yet, if not, added it
    loc = Location(current_x[offset], current_y[offset])
    if loc in presents_delivered:
        presents_delivered[loc] += 1
    else:
        presents_delivered[loc] = 1

# Total number of houses visited is total number of entries in the dictionary
print('Santa and Robo-Santa managed to visit', len(presents_delivered), 'houses!')
