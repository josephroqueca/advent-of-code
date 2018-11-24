#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.read()

import re

# Create regex to match the rules
# 1. Contains at least one pair of two letters that appears twice (non-overlapping)
# 2. At least one letter that repeats, with one letter between them
nicestring_regex = re.compile(r'^(?=\w*(\w)\w\1\w*)(\w*(\w\w)\w*\3\w*)$', flags=re.MULTILINE)
total_nicestrings = len(re.findall(nicestring_regex, puzzle_input))

# Print the total number of nice strings
print('There are', total_nicestrings, 'nice strings!')
