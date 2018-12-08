#!/usr/bin/env python3

import re
import json
import itertools
import hashlib

# Reading test input
def get_test_lines():
    return get_lines(name='../test.txt')

def get_test_file():
    return get_file(name='../test.txt')

# Reading challenge input
def get_file(name='../input.txt'):
    with open(name) as input_file:
        return input_file.read()

def get_lines(name='../input.txt'):
    with open(name) as input_file:
        return input_file.readlines()

def get_nums_by_line():
    return [[int(match) for match in re.findall(r'-?\d+', line)] for line in get_lines()]

def get_test_nums_by_line():
    return [[int(match) for match in re.findall(r'-?\d+', line)] for line in get_test_lines()]

def get_nums_from_line(line):
    return [int(match) for match in re.findall(r'-?\d+', line)]

