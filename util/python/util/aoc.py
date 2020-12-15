import re
import os
import requests
from collections import namedtuple
from enum import Enum


_SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
_session = None


class _Data:
    def __init__(self, contents):
        self._contents = contents

    def contents(self):
        return self._contents[:]

    def lines(self):
        return self._contents.splitlines()

    def numbers(self, allow_negatives=True):
        regex = r'-?\d+' if allow_negatives else r'\d+'
        return [int(re.search(regex, line).group(0)) for line in self.lines()]

    def numbers_by_line(self, allow_negatives=True):
        regex = r'-?\d+' if allow_negatives else r'\d+'
        return [[int(match) for match in re.findall(regex, line)] for line in self.lines()]

    def table(self, data, sep=','):
        return [[
            int(col[1]) if col[0] == 'd' else col[1]
                for col in zip(data, line.split(sep))
        ] for line in self.lines()]

    def parse_lines(self, regex, container=list):
        return [container(re.match(regex, line).groups()) for line in self.lines()]


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

Position = namedtuple('Position', ['x', 'y'])

class Direction(Enum):
    N = (0, -1)
    E = (1, 0)
    S = (0, 1)
    W = (-1, 0)

    @property
    def position(self):
        return Position(self.value[0], self.value[1])
