#!/usr/bin/env python3

import re

# Read the challenge input
with open("input.txt", 'r') as input_file:
  puzzle_input = input_file.readlines()

# Initialize the dictionary of available replacements, and the combinations made
replacements = {}
target_molecule = None
combinations = {}

# For each line in the input
for line in puzzle_input:
  # If the line does not contain '=>', then it is the initial molecule
  if not '=>' in line and len(line) > 1:
    target_molecule = line.rstrip().lstrip()

  # Otherwise, the line represents a replacement. Add it to the available replacements
  elif '=>' in line:
    replacement = re.search(r'(\w+) => (\w+)', line)
    if replacement.group(2) in replacements:
      replacements[replacement.group(2)].append(replacement.group(1))
    else:
      replacements[replacement.group(2)] = [replacement.group(1)]

min_steps = -1
max_steps = 0
potential = [(target_molecule, 0)]
while potential:
  molecule, steps = potential.pop()
  if min_steps != -1 and steps > min_steps:
    continue

  for key in replacements:
    for loc in re.finditer(key, molecule):
      for rep in replacements[key]:

        # Get the new molecule made from the replacement
        replaced = molecule[0:loc.start()] + rep + molecule[loc.start() + len(key):]

        # Ensure a combination is not used more than once
        if replaced in combinations:
          continue
        combinations[replaced] = True

        if replaced == 'e':
          print("Reached 'e' in", steps + 1, "steps.")
          if steps + 1 < min_steps or min_steps == -1:
            min_steps = steps + 1
        else:
          potential.append((replaced, steps + 1))

print("It took a minimum of", min_steps, "to reach 'e'")
