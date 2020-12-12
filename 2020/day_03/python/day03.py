import aoc
from aoc import Position
import math

data = aoc.load(year=2020, day=3)

hill = [list(line) for line in data.lines()]

# Part 1

def count_trees(slope):
    position = Position(0, 0)
    trees = 0
    while position.y < len(hill):
        if hill[position.y][position.x % len(hill[0])] == '#': trees += 1
        position = Position(position.x + slope.x, position.y + slope.y)
    return trees

p1_solution = count_trees(Position(3, 1))
print(p1_solution)

# Part 2

p2_solution = math.prod([count_trees(slope) for slope in [
    Position(1, 1),
    Position(3, 1),
    Position(5, 1),
    Position(7, 1),
    Position(1, 2),
]])
print(p2_solution)
