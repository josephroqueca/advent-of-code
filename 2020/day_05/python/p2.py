#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


# Solution


def get_seat_id(boarding_pass):
    seat_row, seat_col = None, None
    row_lower, row_upper = 0, 127
    col_lower, col_upper = 0, 7

    for index, direction in enumerate(boarding_pass):
        row_mid = (row_lower + row_upper) // 2
        col_mid = (col_lower + col_upper) // 2

        if direction == 'F':
            row_upper = row_mid
            if index == 6:
                seat_row = row_upper
        elif direction == 'B':
            row_lower = row_mid + 1
            if index == 6:
                seat_row = row_lower
        elif direction == 'L':
            col_upper = col_mid
            if index == 9:
                seat_col = col_upper
        elif direction == 'R':
            col_lower = col_mid + 1
            if index == 9:
                seat_col = col_lower

    return seat_row * 8 + seat_col


seat_ids = set([get_seat_id(boarding_pass) for boarding_pass in get_lines()])
seat_id = [seat_id for seat_id in seat_ids if (seat_id + 1) not in seat_ids]
print('Your seat id is {}'.format(seat_id[0] + 1))
