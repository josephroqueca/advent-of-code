#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

possibilities = 0
triangles = [[], [], []]
for line in PUZZLE_INPUT:
    side = line.split()
    for i, _ in enumerate(side):
        triangles[i].append(side[i])
        if len(triangles[i]) == 3:
            possible = int(triangles[i][0]) + int(triangles[i][1]) > int(triangles[i][2])
            possible = int(triangles[i][1]) + int(triangles[i][2]) > int(triangles[i][0]) and possible
            possible = int(triangles[i][0]) + int(triangles[i][2]) > int(triangles[i][1]) and possible
            triangles[i] = []
            if possible:
                possibilities += 1

print('There are', possibilities, 'possible triangles.')
