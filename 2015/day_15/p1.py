##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python cookies.py <input_file>
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

# Array indices
capacity = 0
durability = 1
flavor = 2
texture = 3

# Regular expression to extract information about the ingredients
regex_ingredients = re.compile(r'(\w+): capacity ([-]?\d+), durability ([-]?\d+), flavor ([-]?\d+), texture ([-]?\d+)')
ingredients = {}

# For each line in the input
for line in puzzle_input:
	# Add the ingredient to the list
	stats = re.search(regex_ingredients, line)
	ingredients[stats.group(1)] = [int(stats.group(2)), int(stats.group(3)), int(stats.group(4)), int(stats.group(5))]

# Recursively gets the best possible score from combining ingredients
def get_best_score(amounts, total_used):
	global ingredients

	# If all the ingredients have been used (except one)
	if len(amounts) + 1 == len(ingredients):
		# Cumulative score for each category
		capacity_score = 0
		durability_score = 0
		flavor_score = 0
		texture_score = 0

		# Get the score of each ingredient based on the amount of that ingredient there are
		for ingredient in amounts:
			capacity_score += ingredients[ingredient][capacity] * amounts[ingredient]
			durability_score += ingredients[ingredient][durability] * amounts[ingredient]
			flavor_score += ingredients[ingredient][flavor] * amounts[ingredient]
			texture_score += ingredients[ingredient][texture] * amounts[ingredient]

		# Get the additional scoring from the final ingredient
		for ingredient in ingredients:
			if not ingredient in amounts:
				capacity_score += ingredients[ingredient][capacity] * (100 - total_used)
				durability_score += ingredients[ingredient][durability] * (100 - total_used)
				flavor_score += ingredients[ingredient][flavor] * (100 - total_used)
				texture_score += ingredients[ingredient][texture] * (100 - total_used)

		# Return the product of all scores, replacing the score with 0 if it is less than 0
		return max(capacity_score, 0) * max(durability_score, 0) * max(flavor_score, 0) * max(texture_score, 0)
	else:
		# Find the next unused ingredient and try all the combination of ingredients to get the best score
		for ingredient in ingredients:
			if not ingredient in amounts:
				best_score = 0
				for count in range(101 - total_used):
					updated_amounts = amounts.copy()
					updated_amounts[ingredient] = count
					score = get_best_score(updated_amounts, total_used + count)
					if score > best_score:
						best_score = score
				return best_score

# Pick an ingredient to start and recursively get the best score
best_score = 0
for ingredient in ingredients:
	for count in range(101):
		score = get_best_score({ingredient: count}, count)
		if score > best_score:
			best_score = score
	break

# Print out the best score possible
print('The best possible cookies have a score of', best_score)
