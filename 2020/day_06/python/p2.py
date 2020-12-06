#!/usr/bin/env python3

import re
import functools

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_file():
    with open(FILENAME) as f:
        return f.read()


# Solution


total = sum([
    len(
        functools.reduce(lambda a, b: set(a) & set(b), group.splitlines())
    ) for group in get_file().split('\n\n')
])
print('The sum of the counts is {}'.format(total))
