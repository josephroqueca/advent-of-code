##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python rpg.py

# Initial stats of the player and the boss
boss_base_stats = [109, 8, 2]
player_base_stats = [100, 0, 0]

# Array indices
damage = 1
defense = 2

def battle(boss, player):
	# Get the starting hp of the boss and the player
	boss_hp = boss[0]
	player_hp = player[0]

	# Calculate the damage the player and the boss do
	player_damage = max(1, player[1] - boss[2])
	boss_damage = max(1, boss[1] - player[2])

	# Reduce boss and player hp until one is 0 or less
	while boss_hp > 0 and player_hp > 0:
		boss_hp -= player_damage
		player_hp -= boss_damage

	# True if the player won, false otherwise
	return boss_hp <= 0

# Stats of available weapons
weapons = [
			[8, 4, 0], \
			[10, 5, 0], \
			[25, 6, 0], \
			[40, 7, 0], \
			[74, 8, 0] \
		]

# Stats of available armor
armor = [
			[13, 0, 1], \
			[31, 0, 2], \
			[53, 0, 3], \
			[75, 0, 4], \
			[102, 0, 5] \
		]

# Stats of available rings
rings = [
			[20, 0, 1], \
			[25, 0, 1], \
			[40, 0, 2], \
			[50, 2, 0], \
			[80, 0, 3], \
			[100, 3, 0] \
		]

# The player's starting items for the battle
current_weapon = 0
current_armor = -1
current_ring_one = -1
current_ring_two = -1

player = player_base_stats[:]
maximum_gold = -1
gold = 0

# Iterate through each weapon and combination of items
while current_weapon < len(weapons):
	gold = 0
	player = player_base_stats[:]

	# Exhausting all the combinations of items
	if current_ring_two == len(rings):
		current_ring_two = -1
		current_ring_one += 1
	if current_ring_one == len(rings):
		current_ring_one = -1
		current_armor += 1
	if current_armor == len(armor):
		current_armor = -1
		current_weapon += 1
	if current_weapon == len(weapons):
		break

	# If the two rings are equal, skip the battle because it can't be done
	if current_ring_one == current_ring_two and current_ring_one > -1:
		current_ring_two += 1
		continue

	# Get the gold of the weapon and its additional damage
	gold += weapons[current_weapon][0]
	player[damage] += weapons[current_weapon][damage]

	# Get the gold of the armor and its additional defense
	if current_armor > -1:
		gold += armor[current_armor][0]
		player[defense] += armor[current_armor][defense]
	# Get the gold of the first ring and its additional damage/defense
	if current_ring_one > -1:
		gold += rings[current_ring_one][0]
		player[damage] += rings[current_ring_one][damage]
		player[defense] += rings[current_ring_one][defense]
		# Get the gold of the second ring and its additional damage/defense
	if current_ring_two > -1:
		gold += rings[current_ring_two][0]
		player[damage] += rings[current_ring_two][damage]
		player[defense] += rings[current_ring_two][defense]

	# Check if the boss wins the battle, and compare the gold spent to the maximum
	if not battle(boss_base_stats, player) and gold > maximum_gold:
		maximum_gold = gold

	# Next battle
	current_ring_two += 1

# Print out the minimum gold
print('The shopkeeper persuaded the player to spend', maximum_gold, 'gold to win!')
