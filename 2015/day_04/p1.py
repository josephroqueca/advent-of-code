#!/usr/bin/env python3

from hashlib import md5

# Input from the site
puzzle_input = 'ckczppom'

# Start with checking 1
lowest_positive_int = 1
hashed = md5()
hashed.update(('ckczppom' + str(lowest_positive_int)).encode())
digest = hashed.hexdigest()

# While the first 5 characters are not all 0s, increment the counter and generate a new hash
while digest[:5] != '00000':
  lowest_positive_int += 1
  hashed = md5()
  hashed.update((puzzle_input + str(lowest_positive_int)).encode())
  digest = hashed.hexdigest()

# Print the lowest valid positive integer
print('The lowest positive integer is', lowest_positive_int)
