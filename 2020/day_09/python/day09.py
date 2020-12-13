import aoc
import functools
import itertools

data = aoc.load(year=2020, day=9)
series = data.numbers()

# Part 1

p1_solution = next(
    s for i, s in enumerate(series[25:]) if \
        not any(sum(c) == s for c in itertools.combinations(series[i:i + 25], 2))
)
print(p1_solution)

# Part 2

window = []
for s in series:
    window.append(s)
    while sum(window) > p1_solution:
        window.pop(0)
    if sum(window) == p1_solution:
        p2_solution = min(window) + max(window)
        break
print(p2_solution)
