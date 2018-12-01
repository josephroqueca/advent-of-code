#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

freq = 0
for line in puzzle_input:
    freq += int(line)

print("The resulting frequency is", freq)
