import aoc

data = aoc.load(year=2020, day=10)

joltages = [jolt for jolt in data.as_number_list()]
joltages.extend([0, max(joltages) + 3])

adjacencies = {i: [(i + j + 1) for j in range(3) if (i + j + 1) in joltages] for i in joltages}
joltages = sorted(joltages)

memo = {}
def count_trees(root):
    if root == joltages[-1]: return 1
    if root in memo: return memo[root]

    count = sum([count_trees(adj) for adj in adjacencies[root]])
    memo[root] = count
    return count

solution = count_trees(0)
print(solution)
