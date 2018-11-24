#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.read()

import re

re_markers = re.compile(r'\((\d+)x(\d+)\)')

def get_decompressed_length(text):
  length = 0
  skip_until = 0
  unused_text = text
  for marker in re_markers.finditer(text):
    if marker.start() < skip_until:
      continue

    unused_text = unused_text.replace(text[marker.start():marker.end() + int(marker.group(1))], '', 1)

    repeated_length = int(marker.group(1))
    repetitions = int(marker.group(2))
    repeated_group = text[marker.end():marker.end() + repeated_length]

    decompressed_length = get_decompressed_length(repeated_group)
    additional_length = decompressed_length * repetitions
    length += additional_length

    skip_until = marker.end() + int(marker.group(1))

  length += len(unused_text)
  return length

total_length = get_decompressed_length(puzzle_input)

print('The decompressed length is: {}'.format(total_length))
