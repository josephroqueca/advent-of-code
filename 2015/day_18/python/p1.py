#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

# Initialize the array of lights to all off
lights = [x[:] for x in [[0]*len(puzzle_input)]*len(puzzle_input)]

# For every line in the input
y = 0
for line in puzzle_input:
  line = line.strip()
  x = 0
  for c in line:
    # Set lights which are initially 'on' to 1
    if c == '#': lights[y][x] = 1
    x += 1
  y += 1

# Counts the number of neighbors of a light which are on
def count_neighbors(x, y):
  global lights
  neighbors = 0
  # Loops through all 8 neighbors
  for i in range(9):
    # Skipping the current light
    if i == 4: continue

    # Get the position of the neighbor and check if it is a valid position and on
    yy = y - 1 + int(i / 3)
    xx = x - 1 + i % 3
    if yy >= 0 and yy < len(lights) and xx >= 0 and xx < len(lights[yy]) and lights[yy][xx] == 1:
      neighbors += 1
  return neighbors

# Advance one step
def step():
  global lights

  # Create a copy of the array for the next step
  next_step = [row[:] for row in lights]

  # Loop through each light
  for y in range(len(lights)):
    for x in range(len(lights[y])):

      # Check if the conditions to turn a light on/off are met
      if lights[y][x] == 1 and not count_neighbors(x, y) in [2, 3]:
        next_step[y][x] = 0
      elif lights[y][x] == 0 and count_neighbors(x, y) == 3:
        next_step[y][x] = 1
  lights = next_step

# Step 100 times
for i in range(100):
  step()

# Count the number of lights that are on
total_lights_on = 0
for y in range(len(lights)):
  for x in range(len(lights[y])):
    if lights[y][x] == 1: total_lights_on += 1

# Print the number of lights that are on
print('After 100 steps,', total_lights_on, 'lights are on.')
