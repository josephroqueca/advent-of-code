#!/usr/bin/env python3

import aoc
import re
data = aoc.load(year=2020, day=13)

contents = data.lines()

# Part 1

import itertools

start_time = int(contents[0])
buses = [int(v) for v in re.findall(r'\d+', contents[1])]

for i in itertools.count(start_time):
    departing_bus = next((bid for bid in buses if (start_time + i) % bid == 0), False)
    if departing_bus:
        p1_solution = departing_bus * i
        break

print(p1_solution)

# Part 2

from aoc import chinese_remainder
buses = [int(b) if b != 'x' else b for b in contents[1].split(',')]

n = [bid for bid in buses if bid != 'x']
a = [0] + [bid - (idx + 1) for idx, bid in enumerate(buses[1:]) if bid != 'x']

p2_solution = chinese_remainder(n, a)
print(p2_solution)
