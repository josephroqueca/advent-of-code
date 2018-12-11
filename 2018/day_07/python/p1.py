#!/usr/bin/env python3

import re

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

def get_lines(name=filename):
    with open(name) as input_file:
        return input_file.readlines()

lines = get_lines()

class Step(object):
    def __init__(self, children, parents):
        self.children = children
        self.parents = parents
    def __repr__(self):
        return str(self.children) + " -- " + str(self.parents)

steps = {}

for line in lines:
    line_steps = [match[1] for match in re.findall(r'(S|s)tep (\w)', line)]
    parent_name = line_steps[0]
    child_name = line_steps[1]

    if parent_name in steps:
        steps[parent_name].children.add(child_name)
    else:
        steps[parent_name] = Step(set([child_name]), set())

    if child_name in steps:
        steps[child_name].parents.add(parent_name)
    else:
        steps[child_name] = Step(set(), set([parent_name]))

order = ""
total_steps = len(steps)
while len(order) < total_steps:
    first_available = None
    for step_name in steps:
        step = steps[step_name]
        if not step.parents:
            if first_available is None:
                first_available = step_name
            elif step_name < first_available:
                first_available = step_name
    order += first_available
    del steps[first_available]
    for step_name in steps:
        step = steps[step_name]
        if first_available in step.parents:
            step.parents.remove(first_available)

print('The jobs are completed in the following order:', order)

