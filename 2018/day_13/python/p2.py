#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../{}.txt'.format(script_path, 'input')

def get_lines():
    with open(filename) as f:
        return f.readlines()

path_ids = set(['|', '-'])
curve_ids = set(['\\', '/'])
intersection_ids = set(['+'])
cart_ids = set(['<', '>', '^', 'v'])

paths = {}
carts = {}
cart_last_turn = {}

y = 0
next_cart_id = 0
for line in get_lines():
    for index, c in enumerate(line):
        cell = (index, y)
        if c == ' ':
            paths[cell] = None
        elif c in path_ids:
            paths[cell] = c
        elif c in intersection_ids:
            paths[cell] = c
        elif c in curve_ids:
            paths[cell] = c
        elif c in cart_ids:
            cart = (index, y)
            next_cart_id += 1
            if c == '<' or c == '>':
                paths[cell] = '-'
                carts[cart] = (-1, 0, next_cart_id) if c == '<' else (1, 0, next_cart_id)
            else:
                paths[cell] = '|'
                carts[cart] = (0, -1, next_cart_id) if c == '^' else (0, 1, next_cart_id)
    y += 1

for cart in carts:
    cart_last_turn[carts[cart][2]] = -1

tick = 0
crash_position = None
crashes = {}
while len(carts) > 1:
    cart_cells = sorted(list(carts.keys()), key=lambda x: (x[1], x[0]))
    for cart in cart_cells:
        if cart not in carts:
            continue
        cart_velocity = (carts[cart][0], carts[cart][1])
        cart_id = (carts[cart][2])
        del carts[cart]

        cart_position = (cart[0], cart[1])
        cart_position_next = (cart[0] + cart_velocity[0], cart[1] + cart_velocity[1])
        cart_next_id = (cart_position_next[0], cart_position_next[1])

        if cart_position_next in carts:
            crash_position = cart_position_next
            crashes[crash_position] = True
            del carts[cart_position_next]
            continue

        if cart_velocity[0] == -1 and paths[cart_position_next] in curve_ids:
            if paths[cart_position_next] == '/':
                carts[cart_next_id] = (0, 1, cart_id)
            else:
                carts[cart_next_id] = (0, -1, cart_id)
        elif cart_velocity[0] == 1 and paths[cart_position_next] in curve_ids:
            if paths[cart_position_next] == '/':
                carts[cart_next_id] = (0, -1, cart_id)
            else:
                carts[cart_next_id] = (0, 1, cart_id)
        elif cart_velocity[1] == -1 and paths[cart_position_next] in curve_ids:
            if paths[cart_position_next] == '/':
                carts[cart_next_id] = (1, 0, cart_id)
            else:
                carts[cart_next_id] = (-1, 0, cart_id)
        elif cart_velocity[1] == 1 and paths[cart_position_next] in curve_ids:
            if paths[cart_position_next] == '/':
                carts[cart_next_id] = (-1, 0, cart_id)
            else:
                carts[cart_next_id] = (1, 0, cart_id)
        elif paths[cart_position_next] in intersection_ids:
            cart_last_turn[cart_id] += 1
            if cart_last_turn[cart_id] % 3 == 0:
                if cart_velocity[0] == 0:
                    carts[cart_next_id] = (cart_velocity[1], 0, cart_id)
                else:
                    carts[cart_next_id] = (0, -cart_velocity[0], cart_id)
            elif cart_last_turn[cart_id] % 3 == 1:
                carts[cart_next_id] = (cart_velocity[0], cart_velocity[1], cart_id)
            else:
                if cart_velocity[0] == 0:
                    carts[cart_next_id] = (-cart_velocity[1], 0, cart_id)
                else:
                    carts[cart_next_id] = (0, cart_velocity[0], cart_id)
        else:
            carts[cart_next_id] = (cart_velocity[0], cart_velocity[1], cart_id)

print('The final cart remaining has the following properties:', carts)
