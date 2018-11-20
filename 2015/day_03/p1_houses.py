#!/usr/bin/env python3

from collections import namedtuple

# Read the challenge input
with open("input.txt", 'r') as input_file:
  puzzle_input = input_file.read()

# Tuple to use as dictionary key
Location = namedtuple('Location', ['x', 'y'])

# Start dictionary with Santa's starting location
presents_delivered = {Location(0,0): 1}

# Initialize Santa's starting location
current_x = 0
current_y = 0

# For each character in input
for c in puzzle_input:

  # Move Santa based on the character
  if c == '<': current_x -= 1
  elif c == '>': current_x += 1
  elif c == '^': current_y += 1
  elif c == 'v': current_y -= 1

  # Check if the location is in the dictionary yet, if not, added it
  loc = Location(current_x, current_y)
  if loc in presents_delivered:
    presents_delivered[loc] += 1
  else:
    presents_delivered[loc] = 1

# Total number of houses visited is total number of entries in the dictionary
print('Santa managed to visit', len(presents_delivered), 'houses!')
