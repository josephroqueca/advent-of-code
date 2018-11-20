#!/usr/bin/env python3

# Read the challenge input
with open("input.txt", "r") as input_file:
  puzzle_input = input_file.readlines()

letter_freq = [{} for x in range(len(puzzle_input[0]))]

# Get frequencies of each letter, for each column
for line in puzzle_input:
  for index, letter in enumerate(line):
    if bool(not letter or letter.isspace()): continue
    letter_freq[index][letter] = 1 if letter not in letter_freq[index] else letter_freq[index][letter] + 1

# Iterate over each column's letter frequencies and find the most frequent
secret_word = ""
for column_freq in letter_freq:
  most_frequent_letter = ''
  most_frequent_occurrences = -1
  for letter in column_freq:
    if column_freq[letter] > most_frequent_occurrences:
      most_frequent_letter = letter
      most_frequent_occurrences = column_freq[letter]

  secret_word += most_frequent_letter

print("The secret word is", secret_word)
