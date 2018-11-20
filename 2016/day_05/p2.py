#!/usr/bin/env python3

from hashlib import md5

# Door ID which prefixes hashed value
door_id = 'reyedfim'

# Starting value to hash
hashed_integer = -1

values_found = 0
code = [None, None, None, None, None, None, None, None]
while values_found < 8:
  hashed_integer += 1
  hashed = md5()
  hashed.update((door_id + str(hashed_integer)).encode())
  digest = hashed.hexdigest()

  # While the first 5 characters are not all 0s, increment the counter and generate a new hash
  while digest[:5] != '00000':
    hashed_integer += 1
    hashed = md5()
    hashed.update((door_id + str(hashed_integer)).encode())
    digest = hashed.hexdigest()

  if digest[5].isdigit() and int(digest[5]) < 8 and code[int(digest[5])] is None:
    values_found += 1
    code[int(digest[5])] = digest[6]

# Print the password
print("The password is", "".join(code))
