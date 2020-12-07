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


bags_containing_shiny_gold = set()


def find_bags(root_bag, existing):
    prev = {}
    prev[root_bag] = None
    toVisit = [root_bag]

    def resolve_bags(currentBag):
        bags = set()
        source = prev[currentBag]
        while source:
            bags.add(source)
            source = prev[source]
        return bags

    while toVisit:
        currentBag = toVisit.pop()
        if currentBag in existing or currentBag == 'shiny bag':
            return resolve_bags(currentBag)
        elif currentBag == 'shiny gold':
            return resolve_bags(currentBag)

        for bag in bag_rules[currentBag]:
            if bag in prev: continue
            toVisit.append(bag)
            prev[bag] = currentBag

    return set()


for bag in bag_rules:
    bags_containing_shiny_gold |= find_bags(bag, bags_containing_shiny_gold)


print('{} bags eventually contain at least one shiny gold bag'.format(len(bags_containing_shiny_gold)))
