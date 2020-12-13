import aoc
import functools

data = aoc.load(year=2020, day=6)

# Part 1

p1_solution = sum([
    len(
        functools.reduce(lambda a, b: set(a) | set(b), group.splitlines())
    ) for group in data.contents().split('\n\n')
])
print(p1_solution)

# Part 2

p2_solution = sum([
    len(
        functools.reduce(lambda a, b: set(a) & set(b), group.splitlines())
    ) for group in data.contents().split('\n\n')
])
print(p2_solution)
