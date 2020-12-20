import aoc
import math
import re
from aoc import Position

data = aoc.load(year=2020, day=20)

# Parsing

tiles = {}
for l in data.lines():
    if ':' in l:
        tile_id = int(re.search(r'(\d+)', l).group(0))
        tiles[tile_id] = []
    elif l:
        tiles[tile_id].append([c for c in l])

# Operations

def copy_tile(tile):
    return [[y for y in x] for x in tile]

def rotate(tile):
    return [list(t) for t in list(zip(*tile))[::-1]]

def vertical_flip(tile):
    return tile[::-1]

def horizontal_flip(tile):
    return [t[::-1] for t in tile]

def perform(ops, tile):
    for o in ops:
        if o == 'r':
            tile = rotate(tile)
        elif o == 'h':
            tile = horizontal_flip(tile)
        elif o == 'v':
            tile = vertical_flip(tile)
    return tile

def get_top(tile):
    return tile[0]

def get_bottom(tile):
    return tile[-1]

def get_left(tile):
    return [t[0] for t in tile]

def get_right(tile):
    return [t[-1] for t in tile]

def operations_to_fit_tile(tile, left_fit, top_fit):
    left = get_left(tile)
    bottom = get_bottom(tile)
    right = get_right(tile)
    top = get_top(tile)

    if left_fit is None:
        if top_fit == top: return ()
        if top_fit == bottom: return ('v')
        if top_fit == left: return ('r', 'v')
        if top_fit == right: return ('r')
        if top_fit == top[::-1]: return ('h')
        if top_fit == bottom[::-1]: return ('v', 'h')
        if top_fit == left[::-1]: return ('r', 'v', 'h')
        if top_fit == right[::-1]: return ('r', 'h')
    if top_fit is None:
        if left_fit== top: return ('r', 'v')
        if left_fit == bottom: return ('r', 'v', 'h')
        if left_fit == left: return ()
        if left_fit == right: return ('h')
        if left_fit == top[::-1]: return ('r')
        if left_fit == bottom[::-1]: return ('r', 'h')
        if left_fit == left[::-1]: return ('v')
        if left_fit == right[::-1]: return ('v', 'h')

    if left_fit == left and top_fit == top: return ()
    if left_fit == left[::-1] and top_fit == bottom: return ('v')
    if left_fit == top and top_fit == left: return ('r', 'v')
    if left_fit == top[::-1] and top_fit == right: return ('r')
    if left_fit == right and top_fit == top[::-1]: return ('h')
    if left_fit == right[::-1] and top_fit == bottom[::-1]: return ('v', 'h')
    if left_fit == bottom and top_fit == left[::-1]: return ('r', 'v', 'h')
    if left_fit == bottom[::-1] and top_fit == right[::-1]: return ('r', 'h')
    return None

# Part 1

def fit_next_tile(grid, remaining_tiles, pos):
    if len(grid) == width * height:
        return grid
    if not remaining_tiles:
        return False

    left_fit_id = grid[pos.y * width + pos.x - 1] if pos.x > 0 else None
    top_fit_id = grid[(pos.y - 1) * width + pos.x] if pos.y > 0 else None
    left_fit = get_right(tiles[left_fit_id]) if left_fit_id else None
    top_fit = get_bottom(tiles[top_fit_id]) if top_fit_id else None

    for t in remaining_tiles:
        tile = tiles[t]
        ops = operations_to_fit_tile(tile, left_fit, top_fit)
        if ops is None: continue
        tiles[t] = perform(ops, tile)
        fits = fit_next_tile(grid[:] + [t], remaining_tiles - set([t]), Position(pos.x + 1, pos.y) if pos.x < width - 1 else Position(0, pos.y + 1))
        if fits: return fits

    return False

width = height = math.floor(math.sqrt(len(tiles)))
base_tiles = {t: copy_tile(tiles[t]) for t in tiles}
ops_orientations = [(), ('v'), ('h'), ('v', 'h'), ('r'), ('r', 'v'), ('r', 'h'), ('r', 'v', 'h')]
solved_grid = None

def solve_grid():
    for t in tiles:
        for ops in ops_orientations:
            tiles[t] = perform(ops, copy_tile(base_tiles[t]))
            solved_grid = fit_next_tile([t], set(tiles.keys()) - set([t]), Position(1, 0))
            if solved_grid: return solved_grid

solved_grid = solve_grid()
p1_solution = solved_grid[0] * solved_grid[width - 1] * solved_grid[(height - 1) * width] * solved_grid[(height - 1) * width + width - 1]
print(p1_solution)

# Part 2

tile_height = len(tiles[next(iter(tiles))]) - 2
grid_size = height * tile_height
base_grid = [[] for _ in range(grid_size)]

for i, tid in enumerate(solved_grid):
    tile = [t[1:-1] for t in tiles[tid][1:-1]]
    y = i // width
    for yd, row in enumerate(tile):
        base_grid[y * tile_height + yd].extend(row)

sea_monster = [
    ['-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '#', '-'],
    ['#', '-', '-', '-', '-', '#', '#', '-', '-', '-', '-', '#', '#', '-', '-', '-', '-', '#', '#', '#'],
    ['-', '#', '-', '-', '#', '-', '-', '#', '-', '-', '#', '-', '-', '#', '-', '-', '#', '-', '-', '-'],
]

sea_monster_height = len(sea_monster)
sea_monster_width = len(sea_monster[0])

def mark_sea_monster(grid, x, y):
    for xd in range(sea_monster_width):
        for yd in range(sea_monster_height):
            if sea_monster[yd][xd] != '-' and grid[y + yd][x + xd] != sea_monster[yd][xd]:
                return False
    for xd in range(sea_monster_width):
        for yd in range(sea_monster_height):
            if sea_monster[yd][xd] == '#':
                grid[y + yd][x + xd] = 'O'
    return True

def search_grid(grid):
    has_monsters = False
    for y in range(grid_size - sea_monster_height):
        for x in range(grid_size - sea_monster_width):
            if mark_sea_monster(grid, x, y):
                has_monsters = True
    return has_monsters

def roughness(grid):
    return sum([sum([1 for c in row if c == '#']) for row in grid])

def find_sea_monsters():
    for ops in ops_orientations:
        grid = perform(ops, copy_tile(base_grid))
        if search_grid(grid): return roughness(grid)

p2_solution = find_sea_monsters()
print(p2_solution)
