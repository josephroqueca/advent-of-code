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


joltages = [nums[0] for nums in get_numbers_by_line()]
joltages.extend([0, max(joltages) + 3])

joltage_adjacencies = {i: [(i + j + 1) for j in range(3) if (i + j + 1) in joltages] for i in joltages}
joltages = sorted(joltages)
max_joltage = joltages[-1]


counts = {}
def count_trees(root):
    global counts

    if root == max_joltage:
        return 1
    if root in counts:
        return counts[root]

    count = sum([count_trees(adj) for adj in joltage_adjacencies[root]])
    counts[root] = count
    return count


print('There are {} total distinct arrangements'.format(count_trees(0)))
