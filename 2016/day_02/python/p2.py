#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

# Keypad layout
layout = [
  [   -1,   -1,    1,   -1,   -1],
  [   -1,    2,    3,    4,   -1],
  [    5,    6,    7,    8,    9],
  [   -1,  'A',  'B',  'C',   -1],
  [   -1,   -1,  'D',   -1,   -1],
]

# Initial position (number 5)
start = (2, 0)
code = []

keypad_height = len(layout)
keypad_width = len(layout[0])

# Returns true if a position is valid on the keypad
def is_valid(y, x):
  return x >= 0 and y >= 0 and x < keypad_width and y < keypad_height and layout[y][x] != -1

for line in puzzle_input:
  for char in line:
    if char == 'L' and is_valid(start[0], start[1] - 1):
      start = (start[0], start[1] - 1)
    if char == 'U' and is_valid(start[0] - 1, start[1]):
      start = (start[0] - 1, start[1])
    if char == 'R' and is_valid(start[0], start[1] + 1):
      start = (start[0], start[1] + 1)
    if char == 'D' and is_valid(start[0] + 1, start[1]):
      start = (start[0] + 1, start[1])

  code.append(layout[start[0]][start[1]])

print('The actual code to open the bathroom is', code)