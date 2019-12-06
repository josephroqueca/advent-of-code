#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


re_markers = re.compile(r'\((\d+)x(\d+)\)')

total_length = len(PUZZLE_INPUT)
skip_until = 0
for marker in re_markers.finditer(PUZZLE_INPUT):
    if marker.start() < skip_until:
        continue
    total_length -= len(marker.group(0))
    total_length += int(marker.group(1)) * (int(marker.group(2)) - 1)
    skip_until = marker.end() + int(marker.group(1))

print('The decompressed length is: {}'.format(total_length))
