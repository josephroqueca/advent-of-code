#!/usr/bin/env python3

import os
import re


SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]


# Solution

joltages = sorted([nums[0] for nums in get_numbers_by_line()])
joltages = [0] + joltages + [max(joltages) + 3]
differences = [i - joltages[idx] for idx, i in enumerate(joltages[1:])]
product = differences.count(1) * differences.count(3)
print('The number of 1 differences times 3 differences is', product)
