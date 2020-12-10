#!/usr/bin/env python3

import aoc
data = aoc.load(year=__year__, day=__day__)

contents = data.contents()
lines = data.lines()
numbers = data.numbers()
table = data.table(['w', 'd'], sep=',')
