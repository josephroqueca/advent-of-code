from argparse import ArgumentParser
import os
import requests
import sys


parser = ArgumentParser(description='Advent of Code, Python runner')
parser.add_argument('--session', help='Set your session token')
parser.add_argument('--year', help='Set the challenge year')
parser.add_argument('--day', help='Set the challenge day')
parser.add_argument('--part', help='Set the challenge part')
parser.add_argument('--submit', action='store_true', help='Submit your solution')
parsed = parser.parse_args()

day = int(parsed.day)
day_str = str(day) if day >= 10 else '0{day}'.format(day=day)


# Append challenge scripts to path for import
sys.path.append(os.path.join(
    os.path.dirname(__file__),
    '..',
    '..',
    '..',
    parsed.year,
    'day_{day}'.format(day=day_str),
    'python'
))


# Append data module to path so scripts can import
sys.path.append(os.path.join(
    os.path.dirname(__file__),
    '..',
    'util'
))

import aoc
aoc._session = parsed.session

# Get solution
solution = None
if parsed.part == '1':
    import p1
    solution = p1.solution
elif parsed.part == '2':
    import p2
    solution = p2.solution


# Submit, if argument provided
if parsed.submit:
    # TODO: check response of post request
    if solution is None or solution == '':
        print('Failed to submit solution')
    print('Submitting', solution)
    cookies = {'session': session}
    r = requests.post(
        'https://adventofcode.com/{year}/day/{day}/answer'.format(year=year, day=day),
        cookies=cookies,
        data={"level": int(parsed.part), "answer": solution}
    )
