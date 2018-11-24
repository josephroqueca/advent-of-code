#!/usr/bin/env python3

# Read the challenge input
with open('input.txt', 'r') as input_file:
  puzzle_input = input_file.readlines()

support_ssl= 0

for ip in puzzle_input:
  abas = {}
  babs = {}
  inside_brackets = False
  for letter_idx in range(len(ip) - 2):
    if ip[letter_idx] == '[':
      inside_brackets = True
      continue
    elif ip[letter_idx] == ']':
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
