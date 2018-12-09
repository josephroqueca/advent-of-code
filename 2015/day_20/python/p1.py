#!/usr/bin/env python3

# The original password
puzzle_input = 34000000

# Get a list of all the factors of x
def get_factors(x):
  factors = []

  # All values are divisible by 1
  factors.append(1)
  # Loop until at most the sqrt(x)
  for i in range(2, int(x ** (1.0 / 2.0)) + 1):
    # If the value divides x, add it and the other factor to the list
    if x % i == 0:
      factors.append(i)
      if x / i != i:
        factors.append(int(x / i))

  # All values are divisible by themselves (but 1 has already been added)
  if x > 1:
    factors.append(x)
  return factors

# Returns the first house that receives a number of presents of at least the target value
def find_minimum_house(target):
  i = 0
  while True:
    i += 1
    total = 0
    # For every factor, add the presents delivered to the total
    for factor in get_factors(i):
      total += factor * 10

    # If the total is greater that the target, return the value
    if total >= target:
      return i

# Get the house which receives at least the minimum
house = find_minimum_house(puzzle_input)

# Print the house
print('The lowest house that receives', puzzle_input, 'presents is', house)
