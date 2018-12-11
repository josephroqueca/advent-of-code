#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.read()

# Initial position and direction
pos = (0, 0)
direction = 'N'

# Track which positions were visited
visited = {}
visited_twice_first = None

# Returns the new direction after turning towards the left or right
def turn(facing, towards):
  if towards == "R":
    if facing == 'N': return 'E'
    if facing == 'E': return 'S'
    if facing == 'S': return 'W'
    if facing == 'W': return 'N'
  else:
    if facing == 'N': return 'W'
    if facing == 'W': return 'S'
    if facing == 'S': return 'E'
    if facing == 'E': return 'N'
  return facing

# Returns the new position after moving a distance from the current position in the provided direction
def move(start, direction, distance):
  if direction == 'N': return (start[0], start[1] + distance)
  if direction == 'E': return (start[0] + distance, start[1])
  if direction == 'S': return (start[0], start[1] - distance)
  if direction == 'W': return (start[0] - distance, start[1])
  return start

# For each instruction provided in the input
for instruction in puzzle_input.split('', ')':

  # Turn towards the provided direction
  direction = turn(direction, instruction[0])

  # Move one space at a time so that all positions are recorded and when one is visited twice, break
  distance = int(instruction[1:])
  while distance > 0:
    pos = move(pos, direction, 1)
    distance -= 1
    if pos in visited:
      print('You visited', pos, 'twice!')
      if visited_twice_first is None:
        visited_twice_first = pos
    visited[pos] = True

print('The block visited first was', (abs(visited_twice_first[0]) + abs(visited_twice_first[1])), 'blocks away!')