import re
import os

# Read the challenge input
with open(os.path.join(os.path.dirname(__file__), 'input.txt'), 'r') as input_file:
  puzzle_input = input_file.read()

re_markers = re.compile(r'\((\d+)x(\d+)\)')

total_length = len(puzzle_input)
skip_until = 0
for marker in re_markers.finditer(puzzle_input):
    if marker.start() < skip_until: continue
    total_length -= len(marker.group(0))
    total_length += int(marker.group(1)) * (int(marker.group(2)) - 1)
    skip_until = marker.end() + int(marker.group(1))

print('The decompressed length is: {}'.format(total_length))