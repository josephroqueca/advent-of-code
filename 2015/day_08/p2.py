#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import re

# Regex to find special character sequences
special_regex = re.compile(r'(\"|\\)')

# Initialize counts
total_characters_code = 0
total_characters_memory = 0

# For each line in the input
for line in puzzle_input:
  # Remove whitespace from ends of input
  line = line.strip()

  # Get the total number of raw characters
  total_characters_code += len(line)
  total_characters_memory += len(line) + 2

  # Get all instances of special characters and subtract their length from in memory
  special_characters = re.findall(special_regex, line)
  for result in special_characters:
    # Subtract one less than length to account for character the sequence actually represents
    total_characters_memory += 1

difference = total_characters_memory - total_characters_code
print('The difference between the characters in the memory and code is', difference)
