#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Array indices
capacity = 0
durability = 1
flavor = 2
texture = 3
calorie = 4

# Regular expression to extract information about the ingredients
regex_ingredients = re.compile(
    r'(\w+): capacity ([-]?\d+), durability ([-]?\d+), flavor ([-]?\d+), texture ([-]?\d+), calories ([-]?\d+)'
)
ingredients = {}

# For each line in the input
for line in PUZZLE_INPUT:
    # Add the ingredient to the list
    stats = re.search(regex_ingredients, line)
    ingredients[stats.group(1)] = [
        int(stats.group(2)),
        int(stats.group(3)),
        int(stats.group(4)),
        int(stats.group(5)),
        int(stats.group(6))
    ]


def get_best_score(amounts, total_used):
    # Recursively gets the best possible score from combining ingredients
    global ingredients

    # If all the ingredients have been used (except one)
    if len(amounts) + 1 == len(ingredients):
        # Cumulative score for each category
        capacity_score = 0
        durability_score = 0
        flavor_score = 0
        texture_score = 0
        calorie_score = 0

        # Get the score of each ingredient based on the amount of that ingredient there are
        for ingredient in amounts:
            capacity_score += ingredients[ingredient][capacity] * amounts[ingredient]
            durability_score += ingredients[ingredient][durability] * amounts[ingredient]
            flavor_score += ingredients[ingredient][flavor] * amounts[ingredient]
            texture_score += ingredients[ingredient][texture] * amounts[ingredient]
            calorie_score += ingredients[ingredient][calorie] * amounts[ingredient]

        # Get the additional scoring from the final ingredient
        for ingredient in ingredients:
            if not ingredient in amounts:
                capacity_score += ingredients[ingredient][capacity] * (100 - total_used)
                durability_score += ingredients[ingredient][durability] * (100 - total_used)
                flavor_score += ingredients[ingredient][flavor] * (100 - total_used)
                texture_score += ingredients[ingredient][texture] * (100 - total_used)
                calorie_score += ingredients[ingredient][calorie] * (100 - total_used)

        # Only update the score if the calorie count is 500
        if calorie_score == 500:
            # Return the product of all scores, replacing the score with 0 if it is less than 0
            return max(capacity_score, 0) * max(durability_score, 0) * max(flavor_score, 0) * max(texture_score, 0)
        return 0
    # Find the next unused ingredient and try all the combination of ingredients to get the best score
    for ingredient in ingredients:
        if not ingredient in amounts:
            best = 0
            for c in range(101 - total_used):
                updated_amounts = amounts.copy()
                updated_amounts[ingredient] = c
                best = max(get_best_score(updated_amounts, total_used + c), best)
            return best
    return 0


# Pick an ingredient to start and recursively get the best score
best_score = 0
for item in ingredients:
    for count in range(101):
        score = get_best_score({item: count}, count)
        if score > best_score:
            best_score = score
    break

# Print out the best score possible
print('The best possible cookies have a score of', best_score)
