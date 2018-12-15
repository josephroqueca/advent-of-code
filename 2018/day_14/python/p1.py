#!/usr/bin/env python3

total_recipes = 323081
recipes = [3, 7]
elves = [0, 1]

def create_new_recipe():
    global recipes
    recipe_sum = recipes[elves[0]] + recipes[elves[1]]
    if recipe_sum >= 10:
        recipes.append(1)
        recipes.append(recipe_sum % 10)
    else:
        recipes.append(recipe_sum)

def reassign_elves():
    global elves
    elves[0] = (elves[0] + recipes[elves[0]] + 1) % len(recipes)
    elves[1] = (elves[1] + recipes[elves[1]] + 1) % len(recipes)

while len(recipes) < total_recipes:
    create_new_recipe()
    reassign_elves()

for i in range(10):
    create_new_recipe()
    reassign_elves()

print('The 10 recipes following the input are:', ''.join([str(x) for x in recipes[total_recipes:total_recipes + 10]]))

