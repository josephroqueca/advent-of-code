#!/usr/bin/env python3

import re
import json
import itertools
import hashlib

test_input = False

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'test' if test_input else 'input')

def get_file():
    with open(filename) as f:
        return f.read()

def get_lines():
    with open(filename) as f:
        return [line.strip() for line in f.readlines()]

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]
