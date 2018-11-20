##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python newrules.py <input_file>
# where <input_file> is the filename with the question's input

import sys
import re

# Check to make sure correct number of arguments supplied
if (len(sys.argv) != 2):
    print('Invalid number of arguments!')
    sys.exit()

# Read the input from the file provided as argument
input_file = open(sys.argv[1])
puzzle_input = input_file.read()
input_file.close()

# Create regex to match the rules
# 1. Contains at least one pair of two letters that appears twice (non-overlapping)
# 2. At least one letter that repeats, with one letter between them
nicestring_regex = re.compile(r'^(?=\w*(\w)\w\1\w*)(\w*(\w\w)\w*\3\w*)$', flags=re.MULTILINE)
total_nicestrings = len(re.findall(nicestring_regex, puzzle_input))

# Print the total number of nice strings
print('There are', total_nicestrings, 'nice strings!')
