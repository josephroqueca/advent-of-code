#!/usr/bin/env python3

import re

# Input from the site
puzzle_input = '1113122113'

# Apply the process 50 times
for i in range(50):
  print(i)
  puzzle_output = ''

  # Get character repeated at start, number of times its repeated and add to output
  while len(puzzle_input) > 0:
    digits = re.search(r'(\d)\1*', puzzle_input)
    puzzle_input = puzzle_input[len(digits.group(0)):]
    puzzle_output = puzzle_output + str(len(digits.group(0))) + str(digits.group(0)[:1])

  # Update input to iterate
  puzzle_input = puzzle_output

# Update final output
puzzle_output = puzzle_input

# Print the final length of the output
print('The length of the result is', len(puzzle_output))
