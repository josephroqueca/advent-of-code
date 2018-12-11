#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

freq = 0
freq_set = {0: True}
repeated_freq = None
while repeated_freq is None:
    for line in puzzle_input:
        freq += int(line)
        if freq in freq_set:
            repeated_freq = freq
            break
        freq_set[freq] = True

print("The first repeated frequency is", repeated_freq)
