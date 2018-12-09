#!/usr/bin/env python3

# Read the challenge input
with open('../input.txt', 'r') as input_file:
  puzzle_input = input_file.read()

import re

# Find all of the digits in the document
results = re.findall('-?\d+', puzzle_input)

# Add all of the values together
total = 0
for value in results:
  total += int(value)

# Print the final total
print ('The sum of all the numbers in the document is', total)
