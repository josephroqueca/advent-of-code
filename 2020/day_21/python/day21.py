import aoc
import re
from collections import defaultdict

data = aoc.load(year=2020, day=21)

allergens = set()
ingredients = defaultdict(int)
possible_pairs = {}

for food in data.lines():
    food_allergens = [f for f in re.search(r'contains (.*)\)$', food).group(1).split(', ')]
    allergens.update(food_allergens)

    food_ingredients = [f for f in re.search(r'^(.*) \(', food).group(1).split(' ')]
    for i in food_ingredients:
        ingredients[i] += 1

    for a in food_allergens:
        if a not in possible_pairs: possible_pairs[a] = set(food_ingredients)
        possible_pairs[a].intersection_update(food_ingredients)

# Part 1

confirmed_pairs = set()
while True:
    na = next((a for a in possible_pairs if a not in confirmed_pairs and len(possible_pairs[a]) == 1), None)
    if not na: break

    ingredient, next_allergen = next(iter(possible_pairs[na])), na
    for a in possible_pairs:
        if a == next_allergen or ingredient not in possible_pairs[a]: continue
        possible_pairs[a].remove(ingredient)
    confirmed_pairs.add(next_allergen)

non_allergenic = set(ingredients.keys()) - set([next(iter(v)) for v in possible_pairs.values()])

p1_solution = sum([ingredients[i] for i in non_allergenic])
print(p1_solution)

# Part 2

p2_solution = ','.join([next(iter(possible_pairs[v])) for v in sorted(possible_pairs.keys())])
print(p2_solution)
