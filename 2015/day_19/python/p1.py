#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Initialize the dictionary of available replacements, and the combinations made
replacements = {}
input_molecule = None
combinations = {}

# FOr each line in the input
for line in PUZZLE_INPUT:
    # If the line does not contain '=>', then it is the initial molecule
    if not '=>' in line and len(line) > 1:
        input_molecule = list(line)
    # Otherwise, the line represents a replacement. Add it to the available replacements
    elif '=>' in line:
        replacement = re.search(r'(\w+) => (\w+)', line)
        if replacement.group(1) in replacements:
            replacements[replacement.group(1)].append(replacement.group(2))
        else:
            replacements[replacement.group(1)] = [replacement.group(2)]

# For each character in the initial molecule
for i, _ in enumerate(input_molecule):
    # If the character can be replaced, add the replacement to the list of combinations made
    if input_molecule[i] in replacements:
        for replacement in replacements[input_molecule[i]]:
            new_combination = ''.join(input_molecule[:i]) + replacement + ''.join(input_molecule[i + 1:])
            combinations[new_combination] = 1
    # If 2 characters in a row can be replaced, add their replacement as well
    # The longest replacement is only 2 chars, so we only need to do this for up to 2
    substring_len_2 = ''.join(input_molecule[i:i + 2])
    if substring_len_2 in replacements:
        for replacement in replacements[substring_len_2]:
            new_combination = ''.join(input_molecule[:i]) + replacement + ''.join(input_molecule[i + 2:])
            combinations[new_combination] = 1

# Print the number of combinations
print('The total number of possible combinations is', len(combinations))
