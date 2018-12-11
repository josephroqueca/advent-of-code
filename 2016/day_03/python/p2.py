#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

possibilities = 0
triangles = [[], [], []]
for line in puzzle_input:
  side = line.split()
  for i in range(len(side)):
    triangles[i].append(side[i])
    if len(triangles[i]) == 3:
      possible = int(triangles[i][0]) + int(triangles[i][1]) > int(triangles[i][2])
      possible = int(triangles[i][1]) + int(triangles[i][2]) > int(triangles[i][0]) and possible
      possible = int(triangles[i][0]) + int(triangles[i][2]) > int(triangles[i][1]) and possible
      triangles[i] = []
      if possible: possibilities += 1

print('There are', possibilities, 'possible triangles.')
