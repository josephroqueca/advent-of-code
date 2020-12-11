import aoc
from aoc import Position

data = aoc.load(year=2020, day=11)

layout = [[l for l in line.strip()] for line in data.lines()]

def is_in_layout(seat):
    return seat.x >= 0 and seat.x < len(layout[0]) and seat.y >= 0 and seat.y < len(layout)

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

def becomes_occupied(seat):
    return layout[seat.y][seat.x] == 'L' and not [adj for adj in adjacent(seat) if layout[adj.y][adj.x] == '#']

def becomes_unoccupied(seat):
    return layout[seat.y][seat.x] == '#' and len([adj for adj in adjacent(seat) if layout[adj.y][adj.x] == '#']) >= 4


last_layout = None
while last_layout != layout:
    next_layout = [l[:] for l in layout]
    for y, _ in enumerate(layout):
        for x, _ in enumerate(layout[y]):
            seat = Position(x, y)
            if becomes_occupied(seat):
                next_layout[y][x] = '#'
            elif becomes_unoccupied(seat):
                next_layout[y][x] = 'L'
    last_layout = layout
    layout = next_layout


solution = sum([sum([1 if s == '#' else 0 for s in row]) for row in layout])
print(solution)
