#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

claimed = {}

for line in puzzle_input:
    comp = line.split()

    left = int(comp[2][:comp[2].index(',')])
    top = int(comp[2][comp[2].index(',')+1:-1])
    width = int(comp[3][:comp[3].index('x'):])
    height = int(comp[3][comp[3].index('x')+1:])

    for x in range(width):
        for y in range(height):
            spot = (left + x, top + y)
            if spot in claimed:
                claimed[spot] += 1
            else:
                claimed[spot] = 1

for line in puzzle_input:
    comp = line.split()

    claim_id = comp[0][1:]
    left = int(comp[2][:comp[2].index(',')])
    top = int(comp[2][comp[2].index(',')+1:-1])
    width = int(comp[3][:comp[3].index('x'):])
    height = int(comp[3][comp[3].index('x')+1:])

    valid = True
    for x in range(width):
        for y in range(height):
            spot = (left + x, top + y)
            if claimed[spot] > 1:
                valid = False
    if valid:
        print('The only claim that doesn\'t overlap with any others is:', claim_id)
        break
