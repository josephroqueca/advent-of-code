#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Initialize dict for reindeers and how far they've travelled
race_length = 2503
reindeers = {}
distances = {}
points = {}

speed = 0
race_time = 1
rest_time = 2
moving = 3
time_moving_or_resting = 4

# For each line in the input
for line in PUZZLE_INPUT:
    # Get the important info from each line
    stats = re.search(r'(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+)', line)
    reindeers[stats.group(1)] = [int(stats.group(2)), int(stats.group(3)), int(stats.group(4)), True, 0]
    distances[stats.group(1)] = 0
    points[stats.group(1)] = 0


def get_furthest_reindeer(dists):
    longest_distance = 0
    winner = []
    for r in dists:
        if dists[r] > longest_distance:
            winner = [r]
            longest_distance = dists[r]
        elif dists[r] == longest_distance:
            winner.append(r)
    return winner


# Iterate through each second of the race
clock = 0
while clock < race_length:
    clock += 1
    # Update each reindeer's status
    for reindeer in reindeers:
        # Increment how long they've been waiting / racing
        reindeers[reindeer][time_moving_or_resting] += 1
        if reindeers[reindeer][moving]:
            # If the reindeer is currently racing, update their distance travelled
            distances[reindeer] += reindeers[reindeer][speed]
            # If they've travelled as long as they can, reset their state and set them to not moving
            if reindeers[reindeer][time_moving_or_resting] == reindeers[reindeer][race_time]:
                reindeers[reindeer][moving] = False
                reindeers[reindeer][time_moving_or_resting] = 0
        # Once they've been resting long enough, start them racing again
        elif not reindeers[reindeer][moving] and \
                reindeers[reindeer][time_moving_or_resting] == reindeers[reindeer][rest_time]:
            reindeers[reindeer][moving] = True
            reindeers[reindeer][time_moving_or_resting] = 0

    winning_reindeer = get_furthest_reindeer(distances)
    for reindeer in winning_reindeer:
        points[reindeer] += 1

# Find the longest distance travelled
most_points = 0
for reindeer in points:
    if points[reindeer] > most_points:
        most_points = points[reindeer]

# Print the output
print('The reindeer in first place has earned', most_points, 'points!')
