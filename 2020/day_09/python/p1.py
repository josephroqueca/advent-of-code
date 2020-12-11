import aoc

data = aoc.load(year=2020, day=9)

preamble_count = 25
series = data.numbers()

def follows_rule(num, preamble):
    for idx, i in enumerate(preamble):
        for j in preamble[idx + 1:]:
            if i != j and i + j == num: return True
    return False

first_invalid = None
for idx, num in enumerate(series[preamble_count:]):
    if not follows_rule(num, series[idx:idx + preamble_count]):
        first_invalid = num
        break

solution = first_invalid
print(solution)
