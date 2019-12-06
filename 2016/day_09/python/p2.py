#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


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


total_length = get_decompressed_length(PUZZLE_INPUT)

print('The decompressed length is: {}'.format(total_length))
