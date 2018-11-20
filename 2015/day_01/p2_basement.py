#!/usr/bin/env python3

# Read the challenge input
with open("input.txt", 'r') as input_file:
  puzzle_input = input_file.read()

# Initialize to floor 0, start at first position
current_floor = 0
position = 1

# For each character in input
for c in puzzle_input:
  # Move up or down one floor based on input
  if c == '(':
    current_floor += 1
  elif c == ')':
    current_floor -= 1

  # When the basement is first reached, print out solution and exit
  if current_floor == -1:
    print("He's made it to the basement at position", position)
    break
  else:
    position += 1
