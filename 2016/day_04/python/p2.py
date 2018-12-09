#!/usr/bin/env python3

# Read the challenge input
with open('../input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

import operator

# Returns the 5 most common letters, sorted by frequency, and alphabetically
def get_most_common(letter_freq):
  frequencies = []

  # Add letter, freq as tuple to list
  # Use negative frequency to sort in descending order
  for letter in letter_freq:
    frequencies.append((letter, -letter_freq[letter]))

  frequencies = sorted(frequencies, key=operator.itemgetter(1, 0))
  return ''.join([x[0] for x in frequencies[:5]])

# Rotate each letter in a sector name until the real name is found
def decrypt_name(checksum_start, sectors):
  sector_name = ''
  rotator_dist = int(sectors[len(sectors) - 1][:checksum_start])
  for sector_idx in range(len(sectors)):
    if sector_idx == len(sectors) - 1:
      continue
    else:
      for letter in sectors[sector_idx]:
        actual_letter = chr(((ord(letter) - ord('a') + rotator_dist) % 26) + ord('a'))
        sector_name += actual_letter
  return sector_name

north_pole_found = False
for line in puzzle_input:
  sectors = line.split('-')
  letter_freq = {}
  for sector_idx in range(len(sectors)):
    if sector_idx < len(sectors) - 1:
      # Count frequency of each letter
      for letter in sectors[sector_idx]:
        if letter in letter_freq:
          letter_freq[letter] += 1
        else:
          letter_freq[letter] = 1
    else:
      # Compare checksum to 5 most common letters and add id to sum if equal
      checksum_start = sectors[sector_idx].index('[')
      sector_id = sectors[len(sectors) - 1][:checksum_start]
      checksum = sectors[sector_idx][checksum_start + 1:checksum_start + 6]
      if checksum == get_most_common(letter_freq):
        sector_name = decrypt_name(checksum_start, sectors)
        if sector_name == 'northpoleobjectstorage':
          # Print where the north pole objects are stored
          print('North Pole objects are stored in room', sector_id)
          north_pole_found = True
          break

if not north_pole_found:
  print('Could not find where the North Pole objects are stored.')
