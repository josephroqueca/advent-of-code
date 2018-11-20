#!/usr/bin/env python3

import re

# Read the challenge input
with open("input.txt", 'r') as input_file:
  puzzle_input = input_file.readlines()

# Initialize to 0 feet of wrapping paper
total_square_feet = 0

# For each line in the input
for line in puzzle_input:

  # Get the integers in each line
  sides = re.split('\D', line)
  sides = [int(i) for i in sides[:3]]

  # Get the surface area of each side and add to total
  first_side = sides[0] * sides[1]
  second_side = sides[1] * sides[2]
  third_side = sides[0] * sides[2]
  total_square_feet += 2 * (first_side + second_side + third_side)

  # Find shortest side and add to total
  if first_side < second_side:
    if first_side < third_side:
      total_square_feet += first_side
    else:
      total_square_feet += third_side
  else:
    if third_side < second_side:
      total_square_feet += third_side
    else:
      total_square_feet += second_side

# Print total square feet of wrapping paper needed
print('The elves will need', total_square_feet, 'square feet of wrapping paper!')
