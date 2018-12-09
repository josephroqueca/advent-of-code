#!/usr/bin/env python3

import re
from collections import defaultdict

def get_lines(name='../input.txt'):
    with open(name) as input_file:
        return input_file.readlines()

def get_nums_by_line():
    return [[int(match) for match in re.findall(r'-?\d+', line)] for line in get_lines()]

class Marble(object):
    def __init__(self, value):
        self.value = value
        self.clockwise = None
        self.counterclockwise = None
    def __repr__(self):
        return "{}, ({}, {})".format(self.value, self.clockwise.value, self.counterclockwise.value)

def remove_marble(marble):
    removed_marble = marble
    for i in range(7):
        removed_marble = removed_marble.counterclockwise
    counterclockwise = removed_marble.counterclockwise
    removed_marble.clockwise.counterclockwise = counterclockwise
    counterclockwise.clockwise = removed_marble.clockwise
    return removed_marble, removed_marble.clockwise

players, last_marble_points = get_nums_by_line()[0]
last_marble_points = last_marble_points * 100

player_points = defaultdict(int)
current_player = 0
highest_marble_placed = 0

current_marble = Marble(0)
current_marble.clockwise = current_marble
current_marble.counterclockwise = current_marble
marbles = { 0: current_marble }

while highest_marble_placed < last_marble_points:
    marble_to_place = Marble(highest_marble_placed + 1)
    if marble_to_place.value % 23 == 0:
        player_points[current_player] += marble_to_place.value
        removed_marble, next_marble = remove_marble(current_marble)
        player_points[current_player] += removed_marble.value
        current_marble = next_marble
    else:
        marbles[marble_to_place.value] = marble_to_place
        marble_to_place.clockwise = current_marble.clockwise.clockwise
        marble_to_place.counterclockwise = current_marble.clockwise
        current_marble.clockwise.clockwise.counterclockwise = marble_to_place
        current_marble.clockwise.clockwise = marble_to_place
        current_marble = marble_to_place

    current_player = (current_player + 1) % players
    highest_marble_placed += 1

print('The player with the highest score has', max(player_points.values()), 'points')