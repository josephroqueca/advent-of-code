#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.read()

import re

# Create regex to match the rules
# 1. Contains at least 3 values
# 2. At least one letter that appears twice in a row
# 3. Does not contain 'ab', 'cd', 'pq', or 'xy'
nicestring_regex = re.compile(r'^(?=\w*(\w)\1)(?!\w*(ab|cd|pq|xy))((\w*[aeiou]\w*){3,})$', flags=re.MULTILINE)
total_nicestrings = len(re.findall(nicestring_regex, puzzle_input))

# Print the total number of nice strings
print('There are', total_nicestrings, 'nice strings!')
