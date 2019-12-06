#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


# Find all of the digits in the document
results = re.findall(r'-?\d+', PUZZLE_INPUT)

# Add all of the values together
total = 0
for value in results:
    total += int(value)

# Print the final total
print('The sum of all the numbers in the document is', total)
