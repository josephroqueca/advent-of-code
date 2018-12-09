#!/usr/bin/env python3

# Read the challenge input
with open('../input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import re

# Regular expression to get the names and happiness changes of each pair
regex_happiness = re.compile(r'(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).')
happiness = {}
possibilities = []

# For every line in input
for line in puzzle_input:
  info = re.match(regex_happiness, line)

  # Check if the person is gaining or losing happiness
  mult = 1
  if info.group(2) == 'lose':
    mult = -1

  # Add the person and their neighbor as an entry in the dict
  if info.group(1) in happiness:
    happiness[info.group(1)][info.group(4)] = mult * int(info.group(3))
  else:
    happiness[info.group(1)] = {info.group(4): mult * int(info.group(3))}

# Finds all the possibilities from a person to neighbors which have not been tried so far
# and adds the total change in happiness together
def calc_possibilities(first_person, person, visited, total_so_far):
  global happiness
  global possibilities
  global best_so_far

  # Make a copy of the list and add a new entry
  visited = visited[:]
  visited.append(person)

  # If all of the people are in the list, add the total change in happiness to the possibilities
  if len(visited) == len(happiness):
    total_so_far += happiness[first_person][person] + happiness[person][first_person]
    possibilities.append(total_so_far)

  # For each person the person can sit beside
  for neighbor in happiness[person]:
    # If they're already in the list, skip them
    if neighbor in visited:
      continue

    # Get all the possibilities of the next person's neighbor
    calc_possibilities(first_person, neighbor, visited, total_so_far + happiness[neighbor][person] + happiness[person][neighbor])

# Start with each person and go around the table, trying every combination
for person in happiness:
  for neighbor in happiness[person]:
    calc_possibilities(person, neighbor, [person], happiness[person][neighbor] + happiness[neighbor][person])

# Print the overall best possibility
print('The best seating arrangement has a combined happiness of', max(possibilities))
