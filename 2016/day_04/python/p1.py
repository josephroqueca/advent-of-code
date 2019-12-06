#!/usr/bin/env python3

import operator
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Returns the 5 most common letters, sorted by frequency, and alphabetically

def get_most_common(freq):
    frequencies = []

    # Add letter, freq as tuple to list
    # Use negative frequency to sort in descending order
    for l in freq:
        frequencies.append((l, -freq[l]))

    frequencies = sorted(frequencies, key=operator.itemgetter(1, 0))
    return ''.join([x[0] for x in frequencies[:5]])


sector_id_sum = 0
for line in PUZZLE_INPUT:
    sectors = line.split('-')
    letter_freq = {}
    for sector_idx, _ in enumerate(sectors):
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
            checksum = sectors[sector_idx][checksum_start + 1:checksum_start + 6]
            if checksum == get_most_common(letter_freq):
                sector_id_sum += int(sectors[sector_idx][:checksum_start])


print('The sum of the sector ids of real rooms is', sector_id_sum)
