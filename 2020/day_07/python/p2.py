#!/usr/bin/env python3

import os
import re
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution


bag_rules = {}
for line in get_lines():
    bag = re.match(r'^(.*?) bags', line).group(1)
    bag_rules[bag] = {}
    for contains in re.findall(r'(no|\d+) (.*?) bags?', line):
        if contains[0] != 'no':
            bag_rules[bag][contains[1]] = int(contains[0])


bags_within = {}
def count_bags_within(root):
    global bags_within

    if root in bags_within:
        return bags_within[root]

    held_bags = bag_rules[root]
    if not held_bags:
        return 0

    total = sum([(count_bags_within(bag) + 1) * held_bags[bag] for bag in held_bags.keys()])
    bags_within[root] = total
    return total


bag_count = count_bags_within('shiny gold')
print('{} individual bags are required within a shiny gold bag'.format(bag_count))

