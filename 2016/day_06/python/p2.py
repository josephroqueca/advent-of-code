#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

letter_freq = [{} for x in range(len(PUZZLE_INPUT[0]))]

# Get frequencies of each letter, for each column
for line in PUZZLE_INPUT:
    for index, letter in enumerate(line):
        if bool(not letter or letter.isspace()):
            continue
        letter_freq[index][letter] = 1 if letter not in letter_freq[index] else letter_freq[index][letter] + 1

# Iterate over each column's letter frequencies and find the least frequent
secret_word = ''
for column_freq in letter_freq:
    least_frequent_letter = ''
    least_frequent_occurrences = -1
    for letter in column_freq:
        if column_freq[letter] < least_frequent_occurrences or least_frequent_occurrences == -1:
            least_frequent_letter = letter
            least_frequent_occurrences = column_freq[letter]

    secret_word += least_frequent_letter

print('The secret word is', secret_word)
