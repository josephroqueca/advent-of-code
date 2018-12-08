#!/usr/bin/env python3

import re

def get_lines(name='../input.txt'):
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

def time_for_step(step_name):
    return ord(step_name) - ord('A') + 61

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

ticks = 0
workers = [None] * 5
time_remaining = {}
empty_workers = list(workers)

def next_available_job():
    available = []
    for step_name in steps:
        if not steps[step_name].parents and step_name not in time_remaining:
            available.append(step_name)
    available.sort()
    return available[0] if available else None

while steps:
    for i in range(len(workers)):
        if workers[i] != None:
            worker_job = workers[i]
            time_remaining[worker_job] -= 1
            if time_remaining[worker_job] == 0:
                workers[i] = None
                del time_remaining[worker_job]
                del steps[worker_job]
                for step_name in steps:
                    if worker_job in steps[step_name].parents:
                        steps[step_name].parents.remove(worker_job)

    for i in range(len(workers)):
        if workers[i] == None:
            job = next_available_job()
            if job is not None:
                workers[i] = job
                time_remaining[job] = time_for_step(job)

    if workers != empty_workers:
        ticks += 1

print('The total time to complete all the jobs is:', ticks)
