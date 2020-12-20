import aoc
import re
import math
from aoc import Position

data = aoc.load(year=2020, day=20, test=True)

parsed = {}
for l in data.lines():
    if ':' in l:
        tile_id = int(re.search(r'(\d+)', l).group(0))
        parsed[tile_id] = []
    elif l:
        parsed[tile_id].append(l)

tiles = {
    t: {
        'left': [x[0] for x in parsed[t]],
        'right': [x[-1] for x in parsed[t]],
        'top': [x for x in parsed[t][0]],
        'bottom': [x for x in parsed[t][-1]],
    } for t in parsed
}

def copy_tile(tile):
    return {t: tile[t][:] for t in tile}

def perform(ops, tid):
    tile = tiles[tid]
    for o in ops:
        if o == 'r':
            rotate(tile)
        elif o == 'h':
            horizontal_flip(tile)
        elif o == 'v':
            vertical_flip(tile)

def rotate(tile):
    left = tile['left'][:]
    tile['left'] = tile['top'][::-1]
    tile['top'] = tile['right']
    tile['right'] = tile['bottom'][::-1]
    tile['bottom'] = left

def vertical_flip(tile):
    tile['top'], tile['bottom'] = tile['bottom'], tile['top']
    tile['left'].reverse()
    tile['right'].reverse()

def horizontal_flip(tile):
    tile['left'], tile['right'] = tile['right'], tile['left']
    tile['top'].reverse()
    tile['bottom'].reverse()

def operations_to_fit_tile(tid, left, top):
    tile = tiles[tid]
    if left is None:
        if top == tile['top']: return ()
        if top == tile['bottom']: return ('v')
        if top == tile['left']: return ('r', 'v')
        if top == tile['right']: return ('r')
        if top == tile['top'][::-1]: return ('h')
        if top == tile['bottom'][::-1]: return ('v', 'h')
        if top == tile['left'][::-1]: return ('r', 'v', 'h')
        if top == tile['right'][::-1]: return ('r', 'h')
    if top is None:
        if left == tile['top']: return ('r', 'v')
        if left == tile['bottom']: return ('r', 'v', 'h')
        if left == tile['left']: return ()
        if left == tile['right']: return ('h')
        if left == tile['top'][::-1]: return ('r')
        if left == tile['bottom'][::-1]: return ('r', 'h')
        if left == tile['left'][::-1]: return ('v')
        if left == tile['right'][::-1]: return ('v', 'h')

    if left == tile['left'] and top == tile['top']: return ()
    if left == tile['left'][::-1] and top == tile['bottom']: return ('v')
    if left == tile['top'] and top == tile['left']: return ('r', 'v')
    if left == tile['top'][::-1] and top == tile['right']: return ('r')
    if left == tile['right'] and top == tile['top'][::-1]: return ('h')
    if left == tile['right'][::-1] and top == tile['bottom'][::-1]: return ('v', 'h')
    if left == tile['bottom'] and top == tile['left'][::-1]: return ('r', 'v', 'h')
    if left == tile['bottom'][::-1] and top == tile['right'][::-1]: return ('r', 'h')
    return None

position = Position(0, 0)
width = height = math.floor(math.sqrt(len(tiles)))
# print(width)

def fit_next_tile(grid, remaining_tiles, pos):
    if len(grid) == width * height:
        return grid
    if not remaining_tiles:
        return False

    # print(grid)
    # print('left', pos.y * width + pos.x - 1)
    # print('top', (pos.y - 1) * width + pos.x)
    left_id = grid[pos.y * width + pos.x - 1] if pos.x > 0 else None
    top_id = grid[(pos.y - 1) * width + pos.x] if pos.y > 0 else None
    # print('ids', left_id, top_id)
    left = tiles[left_id]['right'] if left_id else None
    top = tiles[top_id]['bottom'] if top_id else None
    for t in remaining_tiles:
        ops = operations_to_fit_tile(t, left, top)
        # print(left, top, grid, t, ops)
        if ops is None: continue
        perform(ops, t)
        next_grid = grid[:]
        next_grid.append(t)
        solved = fit_next_tile(next_grid, remaining_tiles - set([t]), Position(pos.x + 1, pos.y) if pos.x < width - 1 else Position(0, pos.y + 1))
        if solved: return solved
    return False


orig = {t: copy_tile(tiles[t]) for t in tiles}
ops_orientations = [(), ('v'), ('h'), ('v', 'h'), ('r'), ('r', 'v'), ('r', 'h'), ('r', 'v', 'h')]
solved_grid = None
for t in orig.keys():
    if solved_grid: break
    for ops in ops_orientations:
        tiles[t] = copy_tile(orig[t])
        perform(ops, t)
        solved_grid = fit_next_tile([t], set(tiles.keys()) - set([t]), Position(1, 0))
        if solved_grid: break

# def print_grid(grid):
#     lines = [[] for x in range(height * len(tiles[next(iter(tiles))]['left']))]
#     for i, t in enumerate(grid):
#         tile = tiles[t]
#         y = i // width
#         for

#     print('===')
#     print('\n'.join(lines))
#     print('===')

# grid = fit_next_tile([], set(tiles.keys()), Position(0, 0))
print(solved_grid)
p1_solution = solved_grid[0] * solved_grid[width - 1] * solved_grid[(height - 1) * width] * solved_grid[(height - 1) * width + width - 1]
print(p1_solution)

