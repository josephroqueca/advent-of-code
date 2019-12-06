#!/usr/bin/env python3

import json
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.read()


PUZZLE_INPUT = json.loads(PUZZLE_INPUT)


def get_sum(obj):
    # Recursively gets the sum of an object in the JSON document
    obj_total = 0

    # If the object is a list, get the subtotal of each item
    if isinstance(obj, list):
        for it in obj:
            obj_total += get_sum(it)

    # If the object is a dictionary, get the subtotal of each item
    # or return 0 if any item has a value of 'red'
    elif isinstance(obj, dict):
        for it in obj:
            if obj[it] == 'red':
                return 0
            obj_total += get_sum(obj[it])

    # If the object is a primitive
    else:
        # If the value is red, return 0
        if obj == 'red':
            return 0

        # Otherwise, try to add the item to the sum or add nothing if its not an integer
        try:
            obj_total += int(obj)
        except ValueError:
            obj_total += 0
            # do nothing
    return obj_total


# For each item in the JSON document, add its subtotal to the total
total = 0
for item in PUZZLE_INPUT:
    total += get_sum(PUZZLE_INPUT[item])

# Print the total
print('The sum of all the non-red numbers in the document is', total)
