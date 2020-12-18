import aoc
from aoc import Position, flatten

data = aoc.load(year=2020, day=17)

# Part 1

lines = data.lines()
active = set(flatten([[Position(x - len(l) // 2, y - len(lines) // 2, 0) for x, c in enumerate(l) if c == '#'] for y, l in enumerate(lines)]))
seen = set(flatten([[Position(x - len(l) // 2, y - len(lines) // 2, 0) for x, c in enumerate(l)] for y, l in enumerate(lines)]))
seen.update(flatten([[a for a in c.adjacent()] for c in seen]))

def cycle(active, seen):
    next_active, next_seen = set(), set(seen)
    for c in seen:
        adj = c.adjacent()
        next_seen.update(adj)
        if c in active and 2 <= len([1 for x in adj if x in active]) <= 3:
            next_active.add(c)
        elif c not in active and len([1 for x in adj if x in active]) == 3:
            next_active.add(c)
    return (next_active, next_seen)

for _ in range(6):
    active, seen = cycle(active, seen)

p1_solution = len(active)
print(p1_solution)

# Part 2

active = set(flatten([[Position(x - len(l) // 2, y - len(lines) // 2, 0, 0) for x, c in enumerate(l) if c == '#'] for y, l in enumerate(lines)]))
seen = set(flatten([[Position(x - len(l) // 2, y - len(lines) // 2, 0, 0) for x, c in enumerate(l)] for y, l in enumerate(lines)]))
seen.update(flatten([[a for a in c.adjacent()] for c in seen]))

for _ in range(6):
    active, seen = cycle(active, seen)

p2_solution = len(active)
print(p2_solution)
