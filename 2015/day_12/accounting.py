##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python accounting.py <input_file>
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

# Find all of the digits in the document
results = re.findall('-?\d+', puzzle_input)

# Add all of the values together
total = 0
for value in results:
	total += int(value)

# Print the final total
print ('The sum of all the numbers in the document is', total)
