#!/usr/bin/env python3

import re
import queue

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'input')

def get_file():
    with open(filename) as f:
        return f.read()

def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]

ROCKY, WET, NARROW = 0, 1, 2
TORCH, CLIMBING, NEITHER = 0, 1, 2

rocky_equip = [TORCH, CLIMBING]
wet_equip = [CLIMBING, NEITHER]
narrow_equip = [TORCH, NEITHER]

torch_region = [ROCKY, NARROW]
climbing_region = [ROCKY, WET]
neither_region = [WET, NARROW]

puzzle_input = get_numbers_from_line(get_file())
depth = puzzle_input[0]
target = (puzzle_input[1], puzzle_input[2])
mouth = (0, 0)
initial_state = (TORCH, mouth)

geologic_indices = {
    mouth: 0,
    target: 0,
}

def geologic_index(region):
    if region in geologic_indices:
        return geologic_indices[region]

    x, y = region
    if y == 0:
        index = x * 16807
    elif x == 0:
        index = y * 48271
    else:
        index = erosion_level((x - 1, y)) * erosion_level((x, y - 1))

    geologic_indices[region] = index
    return index

def erosion_level(region):
    index = geologic_index(region)
    return (index + depth) % 20183

def region_type(region):
    erosion = erosion_level(region)
    return erosion % 3

def neighboring_regions(region):
    x, y = region
    return [x for x in [
        (x - 1, y) if x > 0 else None,
        (x, y - 1) if y > 0 else None,
        (x + 1, y),
        (x, y + 1),
    ] if x is not None]

def neighbors(state):
    equipment, region = state
    rt = region_type(region)
    nrs = neighboring_regions(region)
    nrts = [region_type(x) for x in nrs]
    possible_equipment = rocky_equip if rt == ROCKY else wet_equip if rt == WET else narrow_equip
    possible_regions = torch_region if equipment == TORCH else climbing_region if equipment == CLIMBING else neither_region
    return list(set([(e, region, 7) for e in possible_equipment if e is not equipment] +
                    [(equipment, r, 1) for i, r in enumerate(nrs) if nrts[i] in possible_regions]))

def bfs(initial_state, target):
    q = queue.PriorityQueue()
    dist = { initial_state: 0 }
    prev = { initial_state: None }
    
    q.put((0, initial_state))

    best_time = 99999
    while not q.empty():
        minutes, current_state = q.get()
        ns = neighbors(current_state)

        for ne in ns:
            ne_equipment, ne_region, ne_minutes = ne
            ne_state = (ne_equipment, ne_region)

            alt = minutes + ne_minutes
            if alt >= best_time: continue
            if ne_state not in dist or alt < dist[ne_state]:
                dist[ne_state] = alt
                prev[ne_state] = current_state
                if ne_region == target:
                    if ne_equipment != TORCH:
                        alt += 7
                        dist[(TORCH, ne_region)] = alt
                        prev[(TORCH, ne_region)] = ne_state
                    best_time = alt
                q.put((alt, ne_state))
    return best_time

print('The fewest number of minutes needed to reach the target is:', bfs(initial_state, target))

