#!/usr/bin/env python3

import os
import re
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution


base_instructions = [(line[0:3], int(line[4:])) for line in get_lines()]
jmps_and_nops = [idx for idx, ins in enumerate(base_instructions) if ins[0] == 'jmp' or ins[0] == 'nop']


for idx in jmps_and_nops:
    instructions = base_instructions[:]

    if instructions[idx][0] == 'jmp':
        instructions[idx] = ('nop', instructions[idx][1])
    else:
        instructions[idx] = ('jmp', instructions[idx][1])

    pos = 0
    acc = 0
    seen = set()
    while True:
        if pos >= len(instructions):
            print('The accumulator is at', acc)
            break

        if pos in seen:
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
