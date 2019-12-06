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
        self.held_programs = []

        if len(components) > 2:
            self.held_programs = [name if name[-1] != "," else name[0:-1] for name in components[3:]]

    def total_weight(self):
        weight = self.weight
        sub_program_weights = [program_map[name].total_weight() for name in self.held_programs]
        if not sub_program_weights:
            return weight
        sub_weight = sum(sub_program_weights)
        if sub_weight / len(sub_program_weights) != sub_program_weights[0]:
            print(self.name, self.weight, sub_program_weights)
            print([(name, program_map[name].weight, program_map[name].total_weight()) for name in self.held_programs])
        return weight + sub_weight


program_map = {}
for line in PUZZLE_INPUT:
    if not line:
        continue
    program = Program(line)
    program_map[program.name] = program

sub_programs = {program for program_name in program_map for program in program_map[program_name].held_programs}
base_program_name = [x for x in program_map if x not in sub_programs][0]
base_program = program_map[base_program_name]
base_weight = base_program.total_weight()

print('The weight needs to be', base_weight)
