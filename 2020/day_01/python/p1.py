#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')

def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

# Solution

expenses = {}
for expense_line in get_numbers_by_line():
    expense = expense_line[0]
    expenses[expense] = True

    difference = 2020 - expense
    if difference in expenses:
        print('The two entries that sum to 2020 multiplied together equals {}'.format(difference * expense))
        break
