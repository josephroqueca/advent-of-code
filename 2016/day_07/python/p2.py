#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

support_ssl = 0

for ip in PUZZLE_INPUT:
    abas = {}
    babs = {}
    inside_brackets = False
    for letter_idx in range(len(ip) - 2):
        if ip[letter_idx] == '[':
            inside_brackets = True
            continue
        if ip[letter_idx] == ']':
            inside_brackets = False
            continue

        if ip[letter_idx] == ip[letter_idx + 2] and ip[letter_idx] != ip[letter_idx + 1]:
            code = ip[letter_idx:letter_idx + 3]
            corresponding_code = code[1] + code[0] + code[1]
            if inside_brackets:
                babs[code] = letter_idx
                if corresponding_code in abas:
                    support_ssl += 1
                    break
            else:
                abas[code] = letter_idx
                if corresponding_code in babs:
                    support_ssl += 1
                    break

print(support_ssl, 'IPs support SSL.')
