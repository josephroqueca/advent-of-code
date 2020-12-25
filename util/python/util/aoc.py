import os
import re
import requests
from collections import namedtuple
from enum import Enum
from data import _Data


_SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
_session = None


def _fetch(year, day, input_file):
    cookies = {'session': _session}
    r = requests.get(
        'https://adventofcode.com/{year}/day/{day}/input'.format(year=year, day=day),
        cookies=cookies
    )

    with open(input_file, 'w') as f:
        f.write(r.text)


def load(year, day, test=False):
    day_str = str(day) if day >= 10 else '0{day}'.format(day=day)

    input_file = os.path.join(
        _SCRIPT_PATH,
        '..',
        '..',
        '..',
        str(year),
        'day_{day}'.format(day=day_str),
        'test.txt' if test else 'input.txt'
    )
    if not test and not os.path.exists(input_file):
        _fetch(year, day, input_file)

    contents = None
    with open(input_file) as f:
        contents = f.read()

    if not contents:
        raise Exception('Failed to load input data ({input_file})'.format(input_file=input_file))

    return _Data(contents)


def load_output(year, day, part):
    day_str = str(day) if day >= 10 else '0{day}'.format(day=day)
    output = os.path.join(
        _SCRIPT_PATH,
        '..',
        '..',
        '..',
        str(year),
        'day_{day}'.format(day=day_str),
        'output_{part}.txt'.format(part=part)
    )

    if not os.path.exists(output):
        raise Exception('{year}, {day}, Part {part} does not have a cached output'.format(
            year=year,
            day=day,
            part=part
        ))

    contents = ''
    with open(output) as f:
        contents = f.read()

    return _Data(contents)

# Helpers

import fmath
chinese_remainder = fmath.chinese_remainder
mul_inv = fmath.mul_inv

import comp
Computer = comp.Computer

import position
Position = position.Position

class Direction(Enum):
    N = (0, -1)
    E = (1, 0)
    S = (0, 1)
    W = (-1, 0)

    @property
    def position(self):
        return Position(self.value[0], self.value[1])

def flatten(l):
    return [item for sublist in l for item in sublist]

def find_all(s, f):
    return [m.start() for m in re.finditer(s, f)]
