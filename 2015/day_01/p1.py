#!/usr/bin/env python3

# Read the challenge input
with open("input.txt", 'r') as input_file:
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
