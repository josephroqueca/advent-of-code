import aoc

data = aoc.load(year=2020, day=4)

passport_properties = set(['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'])

passports = [' '.join(c.splitlines()) for c in data.contents().split('\n\n')]
passports = [dict(x.split(':') for x in p.split(' ')) for p in passports]

# Part 1

def passes_basic_validation(passport):
    return (set(passport.keys()) - set(['cid'])) == passport_properties

p1_solution = len([p for p in passports if passes_basic_validation(p)])
print(p1_solution)

# Part 2

import re

validations = [
    lambda p: passes_basic_validation(p),
    lambda p: 1920 <= int(p['byr']) <= 2002,
    lambda p: 2010 <= int(p['iyr']) <= 2020,
    lambda p: 2020 <= int(p['eyr']) <= 2030,
    lambda p: (p['hgt'][3:] == 'cm' and 150 <= int(p['hgt'][0:3]) <= 193) or \
        (p['hgt'][2:] == 'in' and 59 <= int(p['hgt'][0:2]) <= 76),
    lambda p: re.search(r'^#[0-9a-fA-F]{6}$', p['hcl']),
    lambda p: p['ecl'] in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'],
    lambda p: re.search(r'^[0-9]{9}$', p['pid']),
]

def passes_validation(passport):
    return all(v(passport) for v in validations)

p2_solution = len([p for p in passports if passes_validation(p)])
print(p2_solution)