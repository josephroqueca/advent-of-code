#!/usr/bin/env python3

import os

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_file():
    with open(FILENAME) as f:
        return f.read()


sequence = get_file().strip()
base_pattern = [0, 1, 0, -1]
pattern = base_pattern

input_signal = [int(s) for s in sequence]

for _ in range(100):
    for el, _ in enumerate(input_signal):
        pattern = [value for value in base_pattern for i in range(el + 1)]
        input_signal[el] = abs(sum([val * pattern[(dig + 1) % len(pattern)]
                                    for dig, val in enumerate(input_signal)])) % 10

print('The first eight digits are {}'.format(''.join([str(s) for s in input_signal[:8]])))
