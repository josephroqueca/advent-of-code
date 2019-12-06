#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


class Program:
    def __init__(self, params):
        components = params.split()
        self.name = components[0]
        self.weight = int(components[1][1:-1])
        self.heldPrograms = []

        if len(components) > 2:
            self.heldPrograms = [name if name[-1] != "," else name[0:-1] for name in components[3:]]


programMap = {}
for line in PUZZLE_INPUT:
    if not line:
        continue
    program = Program(line)
    programMap[program.name] = program

subPrograms = {program for programName in programMap for program in programMap[programName].heldPrograms}
baseProgram = ''.join([x for x in programMap if x not in subPrograms])
print('The bottom program is', baseProgram)
