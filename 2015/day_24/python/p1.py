#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

import copy
from functools import reduce
from operator import mul

total_weight = 0
weights = []

for line in puzzle_input:
  weight = int(line)
  total_weight += weight
  weights.append(weight)

# Get the weights from heaviest to lightest
weights.sort(reverse=True)

# Goal is to get 3 groups of exactly this weight
target_weight = total_weight // 3

minimum_presents = -1
quantum_entanglement = -1

potential_first_bags = []

def bag_it(weights, add_to_compilation, compilation, bag=[]):
  current_weight = sum(bag)
  if current_weight == target_weight:
    if add_to_compilation:
      compilation.append(bag)
    return True
  if current_weight > target_weight:
    return False

  bagged = False
  for i in range(len(weights)):
    weight = weights[i]
    remaining_weights = weights[i + 1:]
    bagged = bag_it(remaining_weights, add_to_compilation, compilation, bag + [weight]) or bagged
    if not add_to_compilation and bagged: return True
  return bagged

bag_it(weights, True, potential_first_bags)
for first_bag in potential_first_bags:
  if minimum_presents != -1 and minimum_presents < len(first_bag):
    continue

  bag_entanglement = reduce(mul, first_bag)
  if quantum_entanglement != -1 and bag_entanglement > quantum_entanglement:
    continue

  bag = first_bag[:]
  remaining_weights = weights[:]
  while bag:
    remaining_weights.remove(bag.pop())
  if bag_it(remaining_weights, False, None):
    minimum_presents = len(first_bag)
    quantum_entanglement = bag_entanglement

print("The best quantum entanglement achievable is", quantum_entanglement)