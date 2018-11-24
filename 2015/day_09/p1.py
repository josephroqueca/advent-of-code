#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import re

# Regex to find special character sequences
city_regex = re.compile(r'(\w+) to (\w+)')
distance_regex = re.compile(r'= (\d+)')

# Intialize empty dictionary of distances
distances = {}

# Gets the shortest distance from one city through those not yet visited
def get_shortest_distance_from_start(distances, start_dest, visited):

  # For every city that start_dest connects to, get the shortest distance for it to travel
  # through the rest of the cities, then add the distance to travel to that city.
  shortest_path = -1
  visited.append(start_dest)
  for next_dest in distances[start_dest]:
    if not next_dest in visited:
      distance = get_shortest_distance_from_start(distances, next_dest, visited[:]) + distances[start_dest][next_dest]
      if shortest_path == -1 or distance < shortest_path:
        shortest_path = distance

  # If all cities were already visited, return 0, otherwise return distance to visit
  return max(shortest_path, 0)

# For each line in the input
for line in puzzle_input:
  # Get the names of the cities
  cities = re.search(city_regex, line)
  distance = re.search(distance_regex, line)

  # Add cities and distances to dictionary
  if cities.group(1) in distances:
    distances[cities.group(1)][cities.group(2)] = int(distance.group(1))
  else:
    distances[cities.group(1)] = {cities.group(2): int(distance.group(1))}
  if cities.group(2) in distances:
    distances[cities.group(2)][cities.group(1)] = int(distance.group(1))
  else:
    distances[cities.group(2)] = {cities.group(1): int(distance.group(1))}

# Find the shortest path from each starting city and compare to get the shortest overall
shortest_path = -1
for starting_point in distances:
  shortest_from_starting = get_shortest_distance_from_start(distances, starting_point, [])
  if shortest_path == -1 or shortest_from_starting < shortest_path:
    shortest_path = shortest_from_starting

# Print the shortest path
print('The shortest distance Santa can travel is', shortest_path, 'km')
