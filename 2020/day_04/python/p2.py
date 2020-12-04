#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


chunk = {}
passports = []
for line in get_lines():
    if len(line) == 0:
        passports.append(chunk)
        chunk = {}
    for match in re.findall(r'([^ ]+):([^ ]+)', line):
        key, value = match[0], match[1]
        if key == 'cid':
            continue
        chunk[key] = value
passports.append(chunk)


expected_fields = set(['ecl', 'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'pid'])
def is_valid(p):
    if set(p.keys()) != expected_fields:
        return False

    if not (len(p['byr']) == 4 and 1920 <= int(p['byr']) <= 2002):
        return False

    if not (len(p['iyr']) == 4 and 2010 <= int(p['iyr']) <= 2020):
        return False

    if not (len(p['eyr']) == 4 and 2020 <= int(p['eyr']) <= 2030):
        return False

    hgt = re.match(r'^(\d+)(in|cm)$', p['hgt'])
    if not hgt: return False
    hgt_value, unit = int(hgt.group(1)), hgt.group(2)
    if unit == 'cm' and not (150 <= hgt_value <= 193):
        return False
    if unit == 'in' and not (59 <= hgt_value <= 76):
        return False

    if not re.match(r'^#[0-9a-fA-F]{6}', p['hcl']):
        return False

    if p['ecl'] not in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']:
        return False

    if not re.match(r'^[0-9]{9}$', p['pid']):
        return False

    return True


total_valid = len([p for p in passports if is_valid(p)])
print('There are {} valid passports.'.format(total_valid))
