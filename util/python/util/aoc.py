import re
import os
import requests


_SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
_session = None


class _Data(object):
    def __init__(self, contents):
        self._contents = contents

    def contents(self):
        return self._contents[:]

    def lines(self):
        return self._contents.splitlines()

    def as_number_list(self, allow_negatives=True):
        regex = r'-?\d+' if allow_negatives else r'\d+'
        return [int(re.search(regex, line).group(0)) for line in self.lines()]


def _fetch(year, day, input_file):
    cookies = {'session': _session}
    r = requests.get(
        'https://adventofcode.com/{year}/day/{day}/input'.format(year=year, day=day),
        cookies=cookies
    )

    with open(input_file, 'w') as f:
        f.write(r.text)


def load(year, day):
    day_str = str(day) if day >= 10 else '0{day}'.format(day=day)

    input_file = os.path.join(_SCRIPT_PATH, '..', '..', '..', str(year), 'day_{day}'.format(day=day_str), 'input.txt')
    if not os.path.exists(input_file):
        _fetch(year, day, input_file)

    contents = None
    with open(input_file) as f:
        contents = f.read()

    if not contents:
        raise Exception('Failed to load input data ({input_file})'.format(input_file=input_file))

    return _Data(contents)
