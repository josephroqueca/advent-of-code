import aoc
import re
from aoc import Position, flatten
from copy import copy

Position.hexagonal = True

data = aoc.load(year=2020, day=24)

# Part 1

black = set()
for tile in data.lines():
    to_flip = Position(0, 0, 0)
    for c in re.findall(r'e|w|se|sw|ne|nw', tile):
        if c == 'e':
            to_flip.move_east()
        elif c == 'w':
            to_flip.move_west()
        elif c == 'ne':
            to_flip.move_northeast()
        elif c == 'nw':
            to_flip.move_northwest()
        elif c == 'se':
            to_flip.move_southeast()
        elif c == 'sw':
            to_flip.move_southwest()

    black.add(to_flip) if to_flip not in black else black.remove(to_flip)

p1_solution = len(black)
print(p1_solution)

# Part 2

def cycle(black, seen):
    next_black = set()
    for tile in seen:
        adj = tile.adjacent()

        adjacent_black = len([1 for x in adj if x in black])
        if tile in black and not (adjacent_black == 0 or adjacent_black > 2):
            next_black.add(tile)
        if tile not in black and adjacent_black == 2:
            next_black.add(tile)
    return (next_black, set(flatten([c.adjacent() for c in next_black])))

seen = black | set(flatten([c.adjacent() for c in black]))
for _ in range(100):
    black, seen = cycle(black, seen)

p2_solution = len(black)
print(p2_solution)
