#!/usr/bin/env python3

recipe_pattern = [3, 2, 3, 0, 8, 1]
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


def has_found_pattern():
    last_recipes = recipes[-8:]
    if len(last_recipes) < len(recipe_pattern):
        return False

    x, y = 0, 0
    matches = 0
    while x < len(recipe_pattern) and y < x + 2:
        if recipe_pattern[x] == last_recipes[y]:
            x, y, matches = x + 1, y + 1, matches + 1
        else:
            y, matches = y + 1, 0
    return matches == len(recipe_pattern)


while not has_found_pattern():
    create_new_recipe()
    reassign_elves()
