import aoc
import re
from aoc import Position, flatten
from copy import copy

data = aoc.load(year=2020, day=24)

def east(pos):
    pos.x, pos.y = pos.x - 1, pos.y + 1
    return pos

def northwest(pos):
    pos.x, pos.z = pos.x + 1, pos.z - 1
    return pos

def northeast(pos):
    pos.y, pos.z = pos.y + 1, pos.z - 1
    return pos

def southwest(pos):
    pos.y, pos.z = pos.y - 1, pos.z + 1
    return pos

def southeast(pos):
    pos.x, pos.z = pos.x - 1, pos.z + 1
    return pos

def west(pos):
    pos.x, pos.y = pos.x + 1, pos.y - 1
    return pos

# Part 1

black = set()
for tile in data.lines():
    to_flip = Position(0, 0, 0)
    for c in re.findall(r'e|w|se|sw|ne|nw', tile):
        if c == 'e':
            east(to_flip)
        elif c == 'w':
            west(to_flip)
        elif c == 'ne':
            northeast(to_flip)
        elif c == 'nw':
            northwest(to_flip)
        elif c == 'se':
            southeast(to_flip)
        elif c == 'sw':
            southwest(to_flip)

    black.add(to_flip) if to_flip not in black else black.remove(to_flip)

p1_solution = len(black)
print(p1_solution)

# Part 2

def adjacent(pos):
    return (
        east(copy(pos)),
        northwest(copy(pos)),
        northeast(copy(pos)),
        southwest(copy(pos)),
        southeast(copy(pos)),
        west(copy(pos)),
    )

def cycle(black, seen):
    next_black = set()
    for tile in seen:
        adj = adjacent(tile)

        adjacent_black = len([1 for x in adj if x in black])
        if tile in black and not (adjacent_black == 0 or adjacent_black > 2):
            next_black.add(tile)
        if tile not in black and adjacent_black == 2:
            next_black.add(tile)
    return (next_black, set(flatten([adjacent(c) for c in next_black])))

seen = black | set(flatten([adjacent(c) for c in black]))
for _ in range(100):
    black, seen = cycle(black, seen)

p2_solution = len(black)
print(p2_solution)
