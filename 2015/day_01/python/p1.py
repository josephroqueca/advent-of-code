#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.read()

# Initialize to floor 0
current_floor = 0

# For each character in input
for c in puzzle_input:

  # Move up or down one floor based on character
  if c == '(':
    current_floor += 1
  elif c == ')':
    current_floor -= 1

# When done, print the current floor
print('Santa is on floor', current_floor)
