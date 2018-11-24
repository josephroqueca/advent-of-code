#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import re

# Array indices
children = 0
cats = 1
samoyeds = 2
pomeranians = 3
akitas = 4
vizslas = 5
goldfish = 6
trees = 7
cars = 8
perfumes = 9

# Properties which the aunt must have more than, less than, or exactly the right amount
greater_than_properties = [cats, trees]
less_than_properties = [pomeranians, goldfish]
equal_properties = [children, samoyeds, akitas, vizslas, cars, perfumes]

# The target properties of the real aunt
aunt_properties = [3, 7, 2, 3, 0, 0, 5, 3, 2, 1]

# Regexes to find the properties of each aunt remembered
aunt_regexes = [r'children: (\d+)', r'cats: (\d+)', r'samoyeds: (\d+)', r'pomeranians: (\d+)', r'akitas: (\d+)', r'vizslas: (\d+)', r'goldfish: (\d+)', r'trees: (\d+)', r'cars: (\d+)', r'perfumes: (\d+)']

# The list of aunts so far
aunts = []

# For each aunt in the input
for line in puzzle_input:
  # Start with -1 for all the values
  aunt = [-1 for i in range(10)]

  # For each property, check if it was remembered about the aunt and, if so, update the list
  for i in range(len(aunt_regexes)):
    match = re.search(aunt_regexes[i], line)
    if match:
      aunt[i] = int(match.group(1))

  # Add the new aunt to the list so far
  aunts.append(aunt)

# Loop through all the aunts and find the one who matches all the properties
# When they are found, print out their number
for aunt in range(len(aunts)):
  the_real_aunt = True
  for i in greater_than_properties:
    if not (aunts[aunt][i] > aunt_properties[i] or aunts[aunt][i] == -1):
      the_real_aunt = False
  for i in less_than_properties:
    if not (aunts[aunt][i] < aunt_properties[i] or aunts[aunt][i] == -1):
      the_real_aunt = False
  for i in equal_properties:
    if not (aunts[aunt][i] == aunt_properties[i] or aunts[aunt][i] == -1):
      the_real_aunt = False
  if the_real_aunt:
    print ('The aunt who sent the gift was Aunt Sue #', aunt + 1)
