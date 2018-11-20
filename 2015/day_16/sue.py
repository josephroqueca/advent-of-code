##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python sue.py <input_file>
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

# The target properties of the real aunt
aunt_properties = [3, 7, 2, 3, 0, 0, 5, 3, 2, 1]

# Regexes to find the properties of each aunt remembered
aunt_regexes = [r'children: (\d+)', r'cats: (\d+)', r'samoyeds: (\d+)', r'pomeranians: (\d+)', r'akitas: (\d+)', r'vizslas: (\d+)', r'goldfish: (\d+)', r'trees: (\d+)', r'cars: (\d+)', r'perfumes: (\d+)']

# The list of aunts so far
aunts = []

# For each aunt in the input
for line in puzzle_input:
	# Start with -1 for all the values
	aunt = [-1 for i in range(10)]

	# For each property, check if it was remembered about the aunt and, if so, update the list
	for i in range(len(aunt_regexes)):
		match = re.search(aunt_regexes[i], line)
		if match:
			aunt[i] = int(match.group(1))

	# Add the new aunt to the list so far
	aunts.append(aunt)

# Loop through all the aunts and find the one who matches all the properties
# When they are found, print out their number
for aunt in range(len(aunts)):
	the_real_aunt = True
	for i in range(len(aunts[aunt])):
		if aunts[aunt][i] != aunt_properties[i] and aunts[aunt][i] != -1:
			the_real_aunt = False
			break
	if the_real_aunt:
		print ('The aunt who sent the gift was Aunt Sue #', aunt + 1)
