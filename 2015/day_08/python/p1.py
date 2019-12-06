#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Regex to find special character sequences
special_regex = re.compile(r'(\\\"|\\x..|\\\\)')

# Initialize counts
total_characters_code = 0
total_characters_memory = 0

# For each line in the input
for line in PUZZLE_INPUT:
    # Remove whitespace from ends of input
    line = line.strip()

    # Get the total number of raw characters
    total_characters_code += len(line)
    total_characters_memory += len(line) - 2

    # Get all instances of special characters and subtract their length from in memory
    special_characters = re.findall(special_regex, line)
    for result in special_characters:
        # Subtract one less than length to account for character the sequence actually represents
        total_characters_memory -= len(result) - 1

difference = total_characters_code - total_characters_memory
print('The difference between the characters in the code and memory is', difference)
