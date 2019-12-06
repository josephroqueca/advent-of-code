#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


# Create regex to match the rules
# 1. Contains at least one pair of two letters that appears twice (non-overlapping)
# 2. At least one letter that repeats, with one letter between them
nicestring_regex = re.compile(r'^(?=\w*(\w)\w\1\w*)(\w*(\w\w)\w*\3\w*)$', flags=re.MULTILINE)
total_nicestrings = len(re.findall(nicestring_regex, PUZZLE_INPUT))

# Print the total number of nice strings
print('There are', total_nicestrings, 'nice strings!')
