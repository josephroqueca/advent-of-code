#!/usr/bin/env python3

# Initial stats of the player and the boss
boss_base_stats = {'hp': 58, 'damage': 9}
player_base_stats = {'hp': 50, 'mana': 500}
hard_mode_enabled = True


class Player:
    def __init__(self):
        self.hp = player_base_stats['hp']
        self.mana = player_base_stats['mana']


class Boss:
    def __init__(self):
        self.hp = boss_base_stats['hp']
        self.damage = boss_base_stats['damage']


class Battle:
    def __init__(self):
        self.player = Player()
        self.boss = Boss()
        self.turns = []
        self.poison_turns = 0
        self.shield_turns = 0
        self.regen_turns = 0
        self.total_cost = 0
        self.armor = 0

    def can_cast(self, spell):
        if self.player.mana < spells[spell]['cost']:
            return False
        if spell == 'shield' and self.shield_turns > 1:
            return False
        if spell == 'recharge' and self.regen_turns > 1:
            return False
        if spell == 'poison' and self.poison_turns > 1:
            return False
        return True

    def apply_effects(self):
        if self.regen_turns > 0:
            self.player.mana += spells['recharge']['regen']
            self.regen_turns -= 1
        if self.poison_turns > 0:
            self.boss.hp -= spells['poison']['damage']
            self.poison_turns -= 1
        if self.shield_turns > 0:
            self.armor = spells['shield']['armor']
            self.shield_turns -= 1
        else:
            self.armor = 0

    def apply_hard_mode(self):
        self.player.hp -= 1

    def duplicate(self):
        dup = Battle()
        dup.player.hp = self.player.hp
        dup.player.mana = self.player.mana
        dup.boss.hp = self.boss.hp
        dup.poison_turns = self.poison_turns
        dup.shield_turns = self.shield_turns
        dup.regen_turns = self.regen_turns
        dup.total_cost = self.total_cost
        return dup

    def player_turn(self, spell):
        if hard_mode_enabled:
            self.apply_hard_mode()
            if self.check_game_over():
                return

        self.apply_effects()
        if self.check_game_over():
            return

        self.total_cost += spells[spell]['cost']
        self.player.mana -= spells[spell]['cost']
        if spell == 'missile':
            self.boss.hp -= spells[spell]['damage']
        elif spell == 'drain':
            self.player.hp += spells[spell]['heal']
            self.boss.hp -= spells[spell]['damage']
        elif spell == 'shield':
            self.shield_turns += spells[spell]['turns']
        elif spell == 'poison':
            self.poison_turns += spells[spell]['turns']
        elif spell == 'recharge':
            self.regen_turns += spells[spell]['turns']

    def boss_turn(self):
        self.apply_effects()
        self.player.hp -= self.boss.damage - self.armor

    def check_game_over(self):
        return self.player.hp <= 0 or self.boss.hp <= 0


# Stats of available spells
spells = {
    'missile': {
        'cost': 53,
        'damage': 4
    },
    'drain': {
        'cost': 73,
        'damage': 2,
        'heal': 2
    },
    'shield': {
        'cost': 113,
        'armor': 7,
        'turns': 6
    },
    'poison': {
        'cost': 173,
        'damage': 3,
        'turns': 6
    },
    'recharge': {
        'cost': 229,
        'regen': 101,
        'turns': 5
    }
}
spell_names = list(spells.keys())

minimum_mana = -1
battles = [Battle()]

# Continue attempting spells until all the battles have concluded
while battles:
    battle = battles.pop()

    # No need to continue if the mana cost won't be the minimum
    if minimum_mana != -1 and battle.total_cost > minimum_mana:
        continue

    if battle.check_game_over():
        if battle.boss.hp <= 0:
            minimum_mana = min(minimum_mana, battle.total_cost) if minimum_mana != -1 else battle.total_cost
        continue

    next_battles = [battle.duplicate() for x in spells]
    for index, next_battle in enumerate(next_battles):
        if not next_battle.can_cast(spell_names[index]):
            continue

        next_battle.player_turn(spell_names[index])

        if next_battle.check_game_over():
            if next_battle.boss.hp <= 0:
                minimum_mana = min(minimum_mana, next_battle.total_cost) if minimum_mana != - \
                    1 else next_battle.total_cost
            continue

        next_battle.boss_turn()
        battles.append(next_battle)

# Print out the minimum mana
print('The player only needed to use', minimum_mana, 'mana to win!')
