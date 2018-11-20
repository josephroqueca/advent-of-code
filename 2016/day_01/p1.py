#!/usr/bin/env python3

# Read the challenge input
with open("input.txt", "r") as input_file:
  puzzle_input = input_file.read()

# Initial position and direction
pos = (0, 0)
direction = "N"

# Returns the new direction after turning towards the left or right
def turn(facing, towards):
  if towards == "R":
    if facing == "N": return "E"
    if facing == "E": return "S"
    if facing == "S": return "W"
    if facing == "W": return "N"
  else:
    if facing == "N": return "W"
    if facing == "W": return "S"
    if facing == "S": return "E"
    if facing == "E": return "N"
  return facing

# Returns the new position after moving a distance from the current position in the provided direction
def move(start, direction, distance):
  if direction == "N": return (start[0], start[1] + distance)
  if direction == "E": return (start[0] + distance, start[1])
  if direction == "S": return (start[0], start[1] - distance)
  if direction == "W": return (start[0] - distance, start[1])
  return start

# For each instruction provided in the input
for instruction in puzzle_input.split(", "):

  # Move in the provided direction
  direction = turn(direction, instruction[0])
  pos = move(pos, direction, int(instruction[1:]))

print("Bunny HQ is located at", pos)
print("It is", (abs(pos[0]) + abs(pos[1])), "blocks away.")
