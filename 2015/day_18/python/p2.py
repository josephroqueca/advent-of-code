#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

# Initialize the array of lights to all off
lights = [x[:] for x in [[0] * len(PUZZLE_INPUT)] * len(PUZZLE_INPUT)]


def is_vertical_end(yy, line):
    return yy in (0, len(line) - 1)


def is_horizontal_end(xx, line):
    return xx in (0, len(line) - 1)


def read_input():
    # For every line in the input
    global lights
    y = 0
    for line in PUZZLE_INPUT:

        line = line.strip()
        x = 0
        for c in line.strip():
            # Set the corners to be on no matter what
            if is_vertical_end(y, PUZZLE_INPUT) and is_horizontal_end(x, line):
                lights[y][x] = 1
            # Set lights which are initially 'on' to 1
            elif c == '#':
                lights[y][x] = 1
            x += 1
        y += 1


def count_neighbors(x, y):
    # Counts the number of neighbors of a light which are on
    global lights
    neighbors = 0
    # Loops through all 8 neighbors
    for i in range(9):
        # Skipping the current light
        if i == 4:
            continue

        # Get the position of the neighbor and check if it is a valid position and on
        yy = y - 1 + int(i / 3)
        xx = x - 1 + i % 3
        if yy in range(0, len(lights)) and xx in range(0, len(lights[yy])) and lights[yy][xx] == 1:
            neighbors += 1
    return neighbors


def step():
    # Advance one step
    global lights

    # Create a copy of the array for the next step
    next_step = [row[:] for row in lights]

    # Loop through each light
    for y, _ in enumerate(lights):
        for x, _ in enumerate(lights[y]):

            # Skip the corners - they are always on
            if is_vertical_end(y, lights) and is_horizontal_end(x, lights[y]):
                continue

            # Check if the conditions to turn a light on/off are met
            if lights[y][x] == 1 and not count_neighbors(x, y) in [2, 3]:
                next_step[y][x] = 0
            elif lights[y][x] == 0 and count_neighbors(x, y) == 3:
                next_step[y][x] = 1
    lights = next_step


read_input()

# Step 100 times
for _ in range(100):
    step()


def total_lights():
    # Count the number of lights that are on
    total_lights_on = 0
    for y, _ in enumerate(lights):
        for x, _ in enumerate(lights[y]):
            if lights[y][x] == 1:
                total_lights_on += 1
    return total_lights_on


# Print the number of lights that are on
print('After 100 steps,', total_lights(), 'lights are on.')
