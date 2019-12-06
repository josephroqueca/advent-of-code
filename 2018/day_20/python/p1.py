#!/usr/bin/env python3

from enum import Enum

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_file():
    with open(FILENAME) as f:
        return f.read()


class Direction(Enum):
    NORTH = (0, 1)
    SOUTH = (0, -1)
    EAST = (1, 0)
    WEST = (-1, 0)

    @classmethod
    def parse(cls, s):
        if s == 'N':
            return cls.NORTH
        if s == 'S':
            return cls.SOUTH
        if s == 'E':
            return cls.EAST
        if s == 'W':
            return cls.WEST
        return None


class Node:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        if isinstance(other, Node):
            return self.x == other.x and self.y == other.y
        return NotImplemented

    def __hash__(self):
        return hash((self.x, self.y))

    def __str__(self):
        return '({}, {})'.format(self.x, self.y)

    def __repr__(self):
        return self.__str__()

    def add(self, d):
        return Node(self.x + d.value[0], self.y + d.value[1])


raw_instructions = get_file()
current_node = facility_root = Node(0, 0)
previous_root = []
facility_nodes = {current_node: set()}

for c in raw_instructions:
    direction = Direction.parse(c)
    if direction:
        next_node = current_node.add(direction)
        if next_node not in facility_nodes:
            facility_nodes[next_node] = set()
        facility_nodes[current_node].add(next_node)
        facility_nodes[next_node].add(current_node)
        current_node = next_node
    elif c == '(':
        previous_root.append(current_node)
    elif c == ')':
        previous_root.pop()
    elif c == '|':
        current_node = previous_root[-1]

distance_to_root = {
    facility_root: 0
}
to_explore = [facility_root]
visited = set()

while to_explore:
    node = to_explore.pop()
    if node in visited:
        continue

    visited.add(node)
    distance = distance_to_root[node]
    children = facility_nodes[node]
    for child in children:
        if child not in distance_to_root or distance_to_root[child] > distance + 1:
            distance_to_root[child] = distance + 1
        if child not in visited:
            to_explore.insert(0, child)

max_distance = max([distance_to_root[x] for x in distance_to_root])
print('The furthest room is {} doors away.'.format(max_distance))
