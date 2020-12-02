#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution

policy_regex = re.compile(r'(\d+)-(\d+) (\w): (.+)')

def is_valid(line):
    match = policy_regex.match(line)

    lower_limit = int(match.group(1))
    upper_limit = int(match.group(2))
    expected_char = match.group(3)
    password = match.group(4)

    occurrences = password.count(expected_char)
    return lower_limit <= occurrences <= upper_limit

valid_passwords = len([password for password in get_lines() if is_valid(password)])

print('There are {} valid passwords'.format(valid_passwords))
