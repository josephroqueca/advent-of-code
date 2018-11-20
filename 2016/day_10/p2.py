#!/usr/bin/env python3

import re

# Read the challenge input
with open("input.txt", "r") as input_file:
  puzzle_input = input_file.readlines()

# Encapsulates bot instructions
class Bot:
  def __init__(self, number, gives_lower_to_output, lower_to_number, gives_higher_to_output, higher_to_number, chip):
    self.number = number
    self.on_lower_chip = (gives_lower_to_output, lower_to_number)
    self.on_higher_chip = (gives_higher_to_output, higher_to_number)
    self.chip = chip

  # Handle when the bot is given a chip
  def receive_chip(self, chip):
    if self.chip == None:
      self.chip = chip
      return None

    higher_chip = max(self.chip, chip)
    lower_chip = min(self.chip, chip)
    self.chip = None
    return self.on_lower_chip + (lower_chip,) + self.on_higher_chip + (higher_chip,)

# Regular expressions to parse bot setup
re_instruction = re.compile('bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)')
re_initial_chip = re.compile('value (\\d+) goes to bot (\\d+)')

bots = {}
output_bins = {}
chips_to_pass = []

for line in puzzle_input:
  if line[0] == 'b': # Passing instructions
    # Get instructions for bot to pass chips
    instruction = re_instruction.match(line)
    bot_number = int(instruction.group(1))
    lower_to_output = instruction.group(2) == 'output'
    lower_goes_to = int(instruction.group(3))
    higher_to_output = instruction.group(4) == 'output'
    higher_goes_to = int(instruction.group(5))

    # Construct the bot with the given info
    bots[bot_number] = Bot(bot_number, lower_to_output, lower_goes_to, higher_to_output, higher_goes_to, None)
  else: # Initial chip
    # Get the bot's starting chip
    initial_chip = re_initial_chip.match(line)
    chip_number = int(initial_chip.group(1))
    bot_number = int(initial_chip.group(2))

    chips_to_pass.append((False, bot_number, chip_number))

# Iterate over all chips being handled
while chips_to_pass:
  to_pass = chips_to_pass.pop()
  if to_pass[0]: # Pass to output bin
    output_bin = to_pass[1]
    chip = to_pass[2]
    if output_bin in output_bins:
      output_bins[output_bin].append(chip)
    else:
      output_bins[output_bin] = [chip]
  else: # Pass to other bot
    bot = to_pass[1]
    chip = to_pass[2]
    result = bots[bot].receive_chip(chip)
    if result != None:
      # When bots have 2 chips, they give out their chips
      chips_to_pass.append((result[0], result[1], result[2]))
      chips_to_pass.append((result[3], result[4], result[5]))

print('Contents of bin 0:', output_bins[0])
print('Contents of bin 1:', output_bins[1])
print('Contents of bin 2:', output_bins[2])
print('Product of contents:', output_bins[0][0] * output_bins[1][0] * output_bins[2][0])
