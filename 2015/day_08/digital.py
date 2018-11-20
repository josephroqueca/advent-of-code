##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python digital.py <input_file>
# where <input_file> is the filename with the question's input

import sys
import re

# Check to make sure correct number of arguments supplied
if (len(sys.argv) != 2):
    print('Invalid number of arguments!')
    sys.exit()

# Read the input from the file provided as argument
input_file = open(sys.argv[1])
puzzle_input = input_file.readlines()
input_file.close()

# Regex to find special character sequences
special_regex = re.compile(r'(\\\"|\\x..|\\\\)')

# Initialize counts
total_characters_code = 0
total_characters_memory = 0

# For each line in the input
for line in puzzle_input:
    # Remove whitespace from ends of input
    line = line.strip()

    # Get the total number of raw characters
    total_characters_code += len(line)
    total_characters_memory += len(line) - 2

    # Get all instances of special characters and subtract their length from in memory
    special_characters = re.findall(special_regex, line)
    for result in special_characters:
        # Subtract one less than length to account for character the sequence actually represents
        total_characters_memory -= len(result) - 1

difference = total_characters_code - total_characters_memory
print('The difference between the characters in the code and memory is', difference)
