#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

import re

class Program(object):
    def __init__(self, params):
        components = params.split()
        self.name = components[0]
        self.weight = int(components[1][1:-1])
        self.heldPrograms = []

        if len(components) > 2:
            self.heldPrograms = [name if name[-1] != "," else name[0:-1] for name in components[3:]]

    def total_weight(self):
        weight = self.weight
        subProgramWeights = [programMap[name].total_weight() for name in self.heldPrograms]
        if not subProgramWeights:
            return weight
        subWeight = sum(subProgramWeights)
        if subWeight / len(subProgramWeights) != subProgramWeights[0]:
            print(self.name, self.weight, subProgramWeights)
            print([(name, programMap[name].weight, programMap[name].total_weight()) for name in self.heldPrograms])
        return weight + subWeight

programMap = {}
for line in puzzle_input:
    if not line: continue
    program = Program(line)
    programMap[program.name] = program

subPrograms = set([program for programName in programMap for program in programMap[programName].heldPrograms])
baseProgramName = [x for x in programMap if x not in subPrograms][0]
baseProgram = programMap[baseProgramName]
baseProgram.total_weight()
