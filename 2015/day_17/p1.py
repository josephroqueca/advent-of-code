#!/usr/bin/env python3

# Initialize and sort the puzzle input
puzzle_input = [33, 14, 18, 20, 45, 35, 16, 35, 1, 13, 18, 13, 50, 44, 48, 6, 24, 41, 30, 42]
container_sizes = puzzle_input
container_sizes.sort(reverse=True)

# Total amount of eggnog to store
target_eggnog = 150

# Gets the number of arrangements of containers to store the eggnog
# containers_total is the total amount of the containers used so far
# remaining_containers is the unused containers
def get_arrangements(containers_total, remaining_containers):
  arrangements = 0

  # If there are no more containers, then the target amount won't be met
  if len(remaining_containers) == 0:
    return 0

  # Iterate over each of the remaining containers
  for i in range(len(remaining_containers) - 1, -1, -1):
    # If the target eggnog amount is met, then add the arrangement to the total
    if containers_total + remaining_containers[i] == target_eggnog:
      arrangements += 1
    # If the total is too large, then only larger containers are remaining
    elif containers_total + remaining_containers[i] > target_eggnog:
      break
    # If the total is too small, keep iterating over the remaining containers
    elif i > 0:
      arrangements += get_arrangements(containers_total + remaining_containers[i], remaining_containers[:i])

  # Return the total number of arrangements made from this iteration
  return arrangements

# Gets the number of total container arrangements
total_arrangements = get_arrangements(0, puzzle_input)

# Prints the total arrangements of containers
print("There are a total of", total_arrangements, "arrangements of containers to hold", target_eggnog, "litres.")
