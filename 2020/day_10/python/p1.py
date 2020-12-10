import aoc

data = aoc.load(year=2020, day=10)

joltages = sorted([jolt for jolt in data.numbers()])
joltages = [0] + joltages + [max(joltages) + 3]
differences = [i - joltages[idx] for idx, i in enumerate(joltages[1:])]

solution = differences.count(1) * differences.count(3)
print(solution)
