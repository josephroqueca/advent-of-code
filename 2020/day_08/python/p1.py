#!/usr/bin/env python3

import os
import re
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution


instructions = [(line[0:3], int(line[4:])) for line in get_lines()]
pos = 0
acc = 0
seen = set()


while True:
    if pos in seen:
        print('The accumulator is at', acc)
        break
    seen.add(pos)

    ins, value = instructions[pos]
    if ins == 'nop':
        pos += 1
    elif ins == 'acc':
        acc += value
        pos += 1
    elif ins == 'jmp':
        pos += value
