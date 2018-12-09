#!/usr/bin/env python3

# Read the challenge input
with open('../input.txt', 'r') as input_file:
  puzzle_input = input_file.read()

import re

# Screen dimensions and initial configuration (all pixels off)
screen_width = 50
screen_height = 6
screen = [[False for x in range(screen_width)] for x in range(screen_height)]

# Regular expressions for extracting values
re_rect = re.compile('rect (\\d+)x(\\d+)')
re_rotate = re.compile('rotate (row|column) (y|x)=(\\d+) by (\\d+)')

# Iterate over each command
for line in puzzle_input:

  # Light up a rectangle
  match = re_rect.match(line)
  if match != None:
    width = int(match.group(1))
    height = int(match.group(2))
    for x in range(width):
      for y in range(height):
        screen[y][x] = True
    continue

  match = re_rotate.match(line)
  if match != None:
    if match.group(1) == 'row':
      # Rotate a row
      row = int(match.group(3))
      distance = int(match.group(4)) % screen_width
      screen[row] = screen[row][-distance:] + screen[row][0:screen_width - distance]
    else:
      # Rotate a column
      column = int(match.group(3))
      distance = int(match.group(4)) % screen_height
      to_rotate = [screen[x][column] for x in range(screen_height)]
      to_rotate = to_rotate[-distance:] + to_rotate[0:screen_height - distance]
      for x in range(len(to_rotate)): screen[x][column] = to_rotate[x]

# Count lit pixels and output to screen
lit_pixels = 0
for row in screen:
  for lit in row:
    if lit:
      lit_pixels += 1
      print('#', end='')
    else:
      print('.', end='')
  print()

print(lit_pixels, 'pixels are lit.')
