import aoc
import math
from itertools import chain

data = aoc.load(year=2020, day=1)

# Part 1

expenses = set(data.numbers())
p1_solution = math.prod([e for e in expenses if (2020 - e) in expenses])
print(p1_solution)

# Part 2

p2_solution = math.prod(set(chain(*[[e for e in expenses if (2020 - f - e) in expenses] for f in expenses])))
print(p2_solution)
