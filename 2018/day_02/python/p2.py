#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    lines_input = input_file.readlines()

ids_with_pairs = set()
ids_with_triplets = set()
ids = []


def solve_p2():
    for primary_box_id in lines_input:
        for secondary_box_id in lines_input:
            if primary_box_id == secondary_box_id:
                continue
            for i in range(len(primary_box_id)):
                adjusted_primary = primary_box_id[0:i] + primary_box_id[i + 1:]
                adjusted_secondary = secondary_box_id[0:i] + secondary_box_id[i + 1:]
                if adjusted_primary == adjusted_secondary:
                    print('The common letters between your correct box ids are:', adjusted_primary)
                    return


solve_p2()
