#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

possibilities = 0
for line in puzzle_input:
  sides = line.split()
  possible = int(sides[0]) + int(sides[1]) > int(sides[2])
  possible = int(sides[1]) + int(sides[2]) > int(sides[0]) and possible
  possible = int(sides[0]) + int(sides[2]) > int(sides[1]) and possible
  if possible: possibilities += 1

print('There are', possibilities, 'possible triangles.')
