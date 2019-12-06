#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

freq = 0
freq_set = {0: True}
repeated_freq = None
while repeated_freq is None:
    for line in PUZZLE_INPUT:
        freq += int(line)
        if freq in freq_set:
            repeated_freq = freq
            break
        freq_set[freq] = True

print("The first repeated frequency is", repeated_freq)
