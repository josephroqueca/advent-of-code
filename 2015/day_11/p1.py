#!/usr/bin/env python3

import re

# The original password
puzzle_input = ['v', 'z', 'b', 'x', 'k', 'g', 'h', 'b']

# Checks for a row of 3 letters in the password
def three_straight_letters():
  global puzzle_input
  for i in range(len(puzzle_input) - 3):
    for j in range(2):
      if not ord(puzzle_input[i + j]) + 1 == ord(puzzle_input[i + j + 1]):
        break
      if j == 1:
        return True

  return False

# Checks for 2 different sets of doubles in the password
def has_double_doubles():
  global puzzle_input
  double_count = 0
  last_double = None
  for i in range(len(puzzle_input) - 1):
    if puzzle_input[i] != last_double and puzzle_input[i] == puzzle_input[i + 1]:
      double_count += 1
      if double_count == 2:
        return True
      else:
        last_double = puzzle_input[i]
  return False

# Move the letter at position up by 1
# If the letter is 'z', make it 'a' and increment the previous letter
# Skip the letters 'i', 'o' and 'l'
def increment_by_one(position):
  global puzzle_input
  if puzzle_input[position] == 'z':
    puzzle_input[position] = 'a'
    increment_by_one(position - 1)
  else:
    puzzle_input[position] = chr(ord(puzzle_input[position]) + 1)
    if puzzle_input[position] in {'i', 'o', 'l'}:
      puzzle_input[position] = chr(ord(puzzle_input[position]) + 1)

# Skips any letters in the entire string which are 'i', 'o', or 'l'
def increment_all_until_valid():
  global puzzle_input
  for i in range(len(puzzle_input)):
    if puzzle_input[i] in {'i', 'o', 'l'}:
      increment_by_one(i)
      for j in range(i + 1, len(puzzle_input)):
        puzzle_input[j] = 'a'

# Increment until the password is valid
increment_by_one(7)
increment_all_until_valid()
while not has_double_doubles() or not  three_straight_letters():
  increment_by_one(7)

# Printing out the new password
print('Santa\'s new password should be:', puzzle_input)

# Increment until the password is valid
increment_by_one(7)
increment_all_until_valid()
while not has_double_doubles() or not  three_straight_letters():
  increment_by_one(7)

print('Santa needs a new password! Now it\'s', puzzle_input)
