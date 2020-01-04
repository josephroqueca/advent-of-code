#!/usr/bin/env python3

import os

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'input')


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


class Deck:
    def __init__(self, size):
        self.forwards = True
        self.size = size
        self.cards = list(range(size))

    def __str__(self):
        return ' '.join([str(i) for i in self.cards])

    def position_of_card(self, n):
        return self.cards.index(n)

    def cut(self, n):
        self.cards = self.cards[n:] + self.cards[0:n]

    def stack(self):
        self.cards.reverse()

    def deal(self, n):
        from_pos = 0
        to_pos = 0
        new_cards = self.cards[:]
        while from_pos < self.size:
            new_cards[to_pos] = self.cards[from_pos]
            to_pos = (to_pos + n) % self.size
            from_pos += 1
        self.cards = new_cards


deck = Deck(10007)
for line in get_lines():
    if not line:
        continue
    if 'cut' in line:
        at = int(line.split(' ')[1])
        deck.cut(at)
    elif 'deal with' in line:
        to = int(line.split(' ')[3])
        deck.deal(to)
    else:
        deck.stack()

print('Card 2019 is at position {}'.format(deck.position_of_card(2019)))
