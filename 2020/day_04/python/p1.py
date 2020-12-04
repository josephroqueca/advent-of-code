#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


chunk = {}
passports = []
for line in get_lines():
    if len(line) == 0:
        passports.append(chunk)
        chunk = {}
    for match in re.findall(r'([^ ]+):([^ ]+)', line):
        key, value = match[0], match[1]
        if key == 'cid':
            continue
        chunk[key] = value
passports.append(chunk)


expected_fields = set(['ecl', 'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'pid'])
def is_valid(passport):
    return set(passport.keys()) == expected_fields


total_valid = len([p for p in passports if is_valid(p)])
print('There are {} valid passports.'.format(total_valid))
