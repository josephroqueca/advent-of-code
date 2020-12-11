import aoc

data = aoc.load(year=2020, day=9)
p1 = aoc.load_output(year=2020, day=9, part=1)

series = data.numbers()
target = int(p1.contents())

start = 0
end = 2

while start < len(series) - 2:
    window = sum(series[start:end])
    if window < target: end += 1
    if window > target: start += 1
    if window == target: break

solution = min(series[start:end]) + max(series[start:end])
print(solution)
