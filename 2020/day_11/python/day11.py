import aoc
from aoc import Position

data = aoc.load(year=2020, day=11)

# Part 1

def is_in_layout(seat):
    return seat.x in range(len(layout[0])) and seat.y in range(len(layout))

def adjacent(seat):
    return [adj for adj in [
        Position(seat.x - 1, seat.y - 1),
        Position(seat.x - 1, seat.y),
        Position(seat.x - 1, seat.y + 1),
        Position(seat.x, seat.y - 1),
        Position(seat.x, seat.y + 1),
        Position(seat.x + 1, seat.y - 1),
        Position(seat.x + 1, seat.y),
        Position(seat.x + 1, seat.y + 1),
    ] if is_in_layout(adj)]

def becomes_occupied(seat, adjacent):
    return layout[seat.y][seat.x] == 'L' and not [adj for adj in adjacent(seat) if layout[adj.y][adj.x] == '#']

def becomes_unoccupied(seat, adjacent, limit):
    return layout[seat.y][seat.x] == '#' and len([adj for adj in adjacent(seat) if layout[adj.y][adj.x] == '#']) >= limit

layout = [[l for l in line.strip()] for line in data.lines()]
last_layout = None
while last_layout != layout:
    next_layout = [l[:] for l in layout]
    for y, _ in enumerate(layout):
        for x, _ in enumerate(layout[y]):
            seat = Position(x, y)
            if becomes_occupied(seat, adjacent):
                next_layout[y][x] = '#'
            elif becomes_unoccupied(seat, adjacent, 4):
                next_layout[y][x] = 'L'
    last_layout = layout
    layout = next_layout


p1_solution = sum([sum([1 if s == '#' else 0 for s in row]) for row in layout])
print(p1_solution)

# Part 2

def first_occupied(seat, xoff, yoff):
    t = Position(seat.x + xoff, seat.y + yoff)
    if not is_in_layout(t): return False
    return t if layout[t.y][t.x] != '.' else first_occupied(t, xoff, yoff)

def visible_adjacent(seat):
    return [adj for adj in [
        first_occupied(seat, -1, -1),
        first_occupied(seat, -1, 0),
        first_occupied(seat, -1, 1),
        first_occupied(seat, 0, -1),
        first_occupied(seat, 0, 1),
        first_occupied(seat, 1, -1),
        first_occupied(seat, 1, 0),
        first_occupied(seat, 1, 1),
    ] if adj and is_in_layout(adj)]

layout = [[l for l in line.strip()] for line in data.lines()]
last_layout = None
while last_layout != layout:
    next_layout = [l[:] for l in layout]
    for y, _ in enumerate(layout):
        for x, _ in enumerate(layout[y]):
            seat = Position(x, y)
            if becomes_occupied(seat, visible_adjacent):
                next_layout[y][x] = '#'
            elif becomes_unoccupied(seat, visible_adjacent, 5):
                next_layout[y][x] = 'L'
    last_layout = layout
    layout = next_layout


p2_solution = sum([sum([1 if s == '#' else 0 for s in row]) for row in layout])
print(p2_solution)
