#!/usr/bin/env python3

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

# Read the challenge input
with open(filename, 'r') as input_file:
  puzzle_input = input_file.readlines()

support_tls = 0

for ip in puzzle_input:
  inside_brackets = False
  is_valid = False
  for letter_idx in range(len(ip) - 3):
    if ip[letter_idx] == '[':
      inside_brackets = True
      continue
    elif ip[letter_idx] == ']':
      inside_brackets = False
      continue

    if ip[letter_idx] == ip[letter_idx + 3] and ip[letter_idx + 1] == ip[letter_idx + 2] and ip[letter_idx] != ip[letter_idx + 1]:
      if inside_brackets:
        is_valid = False
        break
      elif is_valid < 1:
        is_valid = True

  if is_valid: support_tls += 1

print(support_tls, 'IPs support TLS.')
