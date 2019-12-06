#!/usr/bin/env python3

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()

support_tls = 0

for ip in PUZZLE_INPUT:
    inside_brackets = False
    is_valid = False
    for letter_idx in range(len(ip) - 3):
        if ip[letter_idx] == '[':
            inside_brackets = True
            continue
        if ip[letter_idx] == ']':
            inside_brackets = False
            continue

        if ip[letter_idx] == ip[letter_idx + 3] and \
                ip[letter_idx + 1] == ip[letter_idx + 2] and \
                ip[letter_idx] != ip[letter_idx + 1]:
            if inside_brackets:
                is_valid = False
                break
            if is_valid < 1:
                is_valid = True

    if is_valid:
        support_tls += 1

print(support_tls, 'IPs support TLS.')
