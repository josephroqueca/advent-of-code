#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import re
from math import floor

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
for line in puzzle_input:
  # Get the important info from each line
  stats = re.search(r'(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+)', line)
  reindeers[stats.group(1)] = [int(stats.group(2)), int(stats.group(3)), int(stats.group(4)), True, 0]
  distances[stats.group(1)] = 0
  points[stats.group(1)] = 0

def get_furthest_reindeer(distances):
  longest_distance = 0
  winning_reindeer = []
  for reindeer in distances:
    if distances[reindeer] > longest_distance:
      winning_reindeer = [reindeer]
      longest_distance = distances[reindeer]
    elif distances[reindeer] == longest_distance:
      winning_reindeer.append(reindeer)
  return winning_reindeer

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
    elif not reindeers[reindeer][moving] and reindeers[reindeer][time_moving_or_resting] == reindeers[reindeer][rest_time]:
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
