#!/usr/bin/env python3

import re

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

# Solution
#################################################


def fuel_for_module(mass):
    fuel = mass // 3 - 2
    if fuel <= 0:
        return 0
    return fuel + fuel_for_module(fuel)


total_fuel = sum([fuel_for_module(mass[0]) for mass in get_numbers_by_line()])
print("The total fuel necessary is {}".format(total_fuel))
