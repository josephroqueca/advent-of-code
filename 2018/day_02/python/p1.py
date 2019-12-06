#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    lines_input = input_file.readlines()

ids_with_pairs = set()
ids_with_triplets = set()

for box_id in lines_input:
    id_letters = {}
    for letter in box_id:
        if letter in id_letters:
            id_letters[letter] += 1
        else:
            id_letters[letter] = 1

    for letter in id_letters:
        if id_letters[letter] == 2:
            ids_with_pairs.add(box_id)
        if id_letters[letter] == 3:
            ids_with_triplets.add(box_id)

print('The checksum is', len(ids_with_pairs) * len(ids_with_triplets))
