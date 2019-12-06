#!/usr/bin/env python3

import copy

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


ground = set()
trees = set()
lumber = set()

width = 50
height = 50

lines = get_lines()
for y, line in enumerate(lines):
    for x, c in enumerate(line):
        if c == '.':
            ground.add((x, y))
        elif c == '#':
            lumber.add((x, y))
        elif c == '|':
            trees.add((x, y))

current_state = {
    'ground': ground,
    'trees': trees,
    'lumber': lumber
}


def neighbors(cell):
    n = []
    for xx in range(-1, 2):
        for yy in range(-1, 2):
            if xx == 0 and yy == 0:
                continue
            n.append((cell[0] - xx, cell[1] - yy))
    return n


def total_resources(state):
    total_lumber = 0
    total_trees = 0
    for state_y in range(height):
        for state_x in range(width):
            cell = (state_x, state_y)
            if cell in state['trees']:
                total_trees += 1
            elif cell in state['lumber']:
                total_lumber += 1
    return total_lumber * total_trees


minutes_passed = 0
while minutes_passed < 10:
    next_state = copy.deepcopy(current_state)
    for x in range(width):
        for y in range(height):
            current_cell = (x, y)
            ns = neighbors(current_cell)
            if current_cell in current_state['ground']:
                surrounding_trees = sum([1 if n in current_state['trees'] else 0 for n in ns])
                if surrounding_trees >= 3:
                    next_state['ground'].remove(current_cell)
                    next_state['trees'].add(current_cell)
            elif current_cell in current_state['trees']:
                surrounding_lumber = sum([1 if n in current_state['lumber'] else 0 for n in ns])
                if surrounding_lumber >= 3:
                    next_state['trees'].remove(current_cell)
                    next_state['lumber'].add(current_cell)
            elif current_cell in current_state['lumber']:
                surrounding_lumber = sum([1 if n in current_state['lumber'] else 0 for n in ns])
                surrounding_trees = sum([1 if n in current_state['trees'] else 0 for n in ns])
                if surrounding_lumber == 0 or surrounding_trees == 0:
                    next_state['lumber'].remove(current_cell)
                    next_state['ground'].add(current_cell)

    current_state = next_state
    minutes_passed += 1

print('The total resources after 10 minutes is:', total_resources(current_state))
