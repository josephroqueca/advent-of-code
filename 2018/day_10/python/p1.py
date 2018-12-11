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
        return f.readlines()

def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]

def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]

points = {}
for line in get_numbers_by_line(allow_negatives=True):
    x, y, xx, yy = line
    points[(x,y)] = [(xx, yy)]

xs = [x[0] for x in points.keys()]
ys = [x[0] for x in points.keys()]
left, width = min(xs), max(xs)
top, height = min(ys), max(ys)

def print_lights(points):
    xs = [x[0] for x in points.keys()]
    ys = [x[0] for x in points.keys()]
    left, width = min(xs), max(xs)
    top, height = min(ys), max(ys)
    for y in range(top, height + 1):
        for x in range(left, width + 1):
            if (x, y) in points:
                print('#', end='')
            else:
                print('.', end='')
        print()
    print(seconds)

should_print = False
count = 10
seconds = 0
while True:
    if should_print: print_lights(points)
    updated_points = {}
    for point in points:
        x, y = point
        for velocity in points[point]:
            xx, yy = velocity
            new_point = (x + xx, y + yy)
            if new_point in updated_points:
                updated_points[new_point].append((xx, yy))
            else:
                updated_points[new_point] = [(xx, yy)]
    points = updated_points
    seconds += 1

    count -= 1
    if count == 0:
        count = 10
        ys = [x[0] for x in points.keys()]
        top, height = min(ys), max(ys)
        did_print = should_print
        should_print = abs(top - height) < 100 
        if not should_print and did_print != should_print:
            break
