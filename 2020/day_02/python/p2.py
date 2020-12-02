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

    first_position = int(match.group(1)) - 1
    second_position = int(match.group(2)) - 1
    expected_char = match.group(3)
    password = match.group(4)

    return (password[first_position] == expected_char) ^ (password[second_position] == expected_char)

valid_passwords = len([password for password in get_lines() if is_valid(password)])

print('There are {} valid passwords'.format(valid_passwords))
