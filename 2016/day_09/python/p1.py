#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.read()

import re

re_markers = re.compile(r'\((\d+)x(\d+)\)')

total_length = len(puzzle_input)
skip_until = 0
for marker in re_markers.finditer(puzzle_input):
    if marker.start() < skip_until: continue
    total_length -= len(marker.group(0))
    total_length += int(marker.group(1)) * (int(marker.group(2)) - 1)
    skip_until = marker.end() + int(marker.group(1))

print('The decompressed length is: {}'.format(total_length))
