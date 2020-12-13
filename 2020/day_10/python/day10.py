import aoc

data = aoc.load(year=2020, day=10)

# Part 1

joltages = sorted([jolt for jolt in data.numbers()])
joltages = [0] + joltages + [max(joltages) + 3]
differences = [i - joltages[idx] for idx, i in enumerate(joltages[1:])]

p1_solution = differences.count(1) * differences.count(3)
print(p1_solution)

# Part 2

from functools import lru_cache

adjacencies = {i: [(i + j + 1) for j in range(3) if (i + j + 1) in joltages] for i in joltages}
joltages = sorted(joltages)

@lru_cache
def count_trees(root):
    if root == joltages[-1]: return 1
    return sum([count_trees(adj) for adj in adjacencies[root]])

p2_solution = count_trees(0)
print(p2_solution)
