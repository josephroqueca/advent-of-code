#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Regex to find special character sequences
city_regex = re.compile(r'(\w+) to (\w+)')
distance_regex = re.compile(r'= (\d+)')

# Intialize empty dictionary of distances
city_distances = {}


def get_longest_distance_from_start(distances, start_dest, visited):
    # Gets the longest distance from one city through those not yet visited
    # For every city that start_dest connects to, get the longest distance for it to travel
    # through the rest of the cities, then add the distance to travel to that city.
    path = 0
    visited.append(start_dest)
    for next_dest in distances[start_dest]:
        if not next_dest in visited:
            dist = get_longest_distance_from_start(
                distances, next_dest, visited[:]) + distances[start_dest][next_dest]
            if dist > path:
                path = dist

    # If all cities were already visited, return 0, otherwise return distance to visit
    return path


# For each line in the input
for line in PUZZLE_INPUT:
    # Get the names of the cities
    cities = re.search(city_regex, line)
    distance = re.search(distance_regex, line)

    # Add cities and distances to dictionary
    if cities.group(1) in city_distances:
        city_distances[cities.group(1)][cities.group(2)] = int(distance.group(1))
    else:
        city_distances[cities.group(1)] = {cities.group(2): int(distance.group(1))}
    if cities.group(2) in city_distances:
        city_distances[cities.group(2)][cities.group(1)] = int(distance.group(1))
    else:
        city_distances[cities.group(2)] = {cities.group(1): int(distance.group(1))}

# Find the longest path from each starting city and compare to get the longest overall
longest_path = 0
for starting_point in city_distances:
    longest_from_starting = get_longest_distance_from_start(city_distances, starting_point, [])
    if longest_from_starting > longest_path:
        longest_path = longest_from_starting

# Print the longest path
print('The longest distance Santa can travel is', longest_path, 'km')
