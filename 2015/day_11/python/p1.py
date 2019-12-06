#!/usr/bin/env python3

# The original password
PUZZLE_INPUT = ['v', 'z', 'b', 'x', 'k', 'g', 'h', 'b']


def three_straight_letters():
    # Checks for a row of 3 letters in the password
    global PUZZLE_INPUT
    for i in range(len(PUZZLE_INPUT) - 3):
        for j in range(2):
            if not ord(PUZZLE_INPUT[i + j]) + 1 == ord(PUZZLE_INPUT[i + j + 1]):
                break
            if j == 1:
                return True

    return False


def has_double_doubles():
    # Checks for 2 different sets of doubles in the password
    global PUZZLE_INPUT
    double_count = 0
    last_double = None
    for i in range(len(PUZZLE_INPUT) - 1):
        if PUZZLE_INPUT[i] != last_double and PUZZLE_INPUT[i] == PUZZLE_INPUT[i + 1]:
            double_count += 1
            if double_count == 2:
                return True
            last_double = PUZZLE_INPUT[i]
    return False


def increment_by_one(position):
    # Move the letter at position up by 1
    # If the letter is 'z', make it 'a' and increment the previous letter
    # Skip the letters 'i', 'o' and 'l'
    global PUZZLE_INPUT
    if PUZZLE_INPUT[position] == 'z':
        PUZZLE_INPUT[position] = 'a'
        increment_by_one(position - 1)
    else:
        PUZZLE_INPUT[position] = chr(ord(PUZZLE_INPUT[position]) + 1)
        if PUZZLE_INPUT[position] in {'i', 'o', 'l'}:
            PUZZLE_INPUT[position] = chr(ord(PUZZLE_INPUT[position]) + 1)


def increment_all_until_valid():
    # Skips any letters in the entire string which are 'i', 'o', or 'l'
    global PUZZLE_INPUT
    for i, letter in enumerate(PUZZLE_INPUT):
        if letter in {'i', 'o', 'l'}:
            increment_by_one(i)
            for j in range(i + 1, len(PUZZLE_INPUT)):
                PUZZLE_INPUT[j] = 'a'


# Increment until the password is valid
increment_by_one(7)
increment_all_until_valid()
while not has_double_doubles() or not three_straight_letters():
    increment_by_one(7)

# Printing out the new password
print('Santa\'s new password should be:', PUZZLE_INPUT)
