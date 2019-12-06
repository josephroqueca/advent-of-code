#!/usr/bin/env python3

import os
import re

test_input = False

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../{}.txt'.format(SCRIPT_PATH, 'test' if test_input else 'input')


def get_file():
    with open(FILENAME) as f:
        return f.read()


def get_lines():
    with open(FILENAME) as f:
        return [line.strip() for line in f.readlines()]


def get_numbers_by_line(allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [[int(match) for match in re.findall(regex, line)] for line in get_lines()]


def get_numbers_from_line(line, allow_negatives=True):
    regex = r'-?\d+' if allow_negatives else r'\d+'
    return [int(match) for match in re.findall(regex, line)]


def addr(A, B, C, reg):
    reg[C] = reg[A] + reg[B]
    return reg


def addi(A, B, C, reg):
    reg[C] = reg[A] + B
    return reg


def mulr(A, B, C, reg):
    reg[C] = reg[A] * reg[B]
    return reg


def muli(A, B, C, reg):
    reg[C] = reg[A] * B
    return reg


def banr(A, B, C, reg):
    reg[C] = reg[A] & reg[B]
    return reg


def bani(A, B, C, reg):
    reg[C] = reg[A] & B
    return reg


def borr(A, B, C, reg):
    reg[C] = reg[A] | reg[B]
    return reg


def bori(A, B, C, reg):
    reg[C] = reg[A] | B
    return reg


def setr(A, _, C, reg):
    reg[C] = reg[A]
    return reg


def seti(A, _, C, reg):
    reg[C] = A
    return reg


def gtir(A, B, C, reg):
    reg[C] = 1 if A > reg[B] else 0
    return reg


def gtri(A, B, C, reg):
    reg[C] = 1 if reg[A] > B else 0
    return reg


def gtrr(A, B, C, reg):
    reg[C] = 1 if reg[A] > reg[B] else 0
    return reg


def eqir(A, B, C, reg):
    reg[C] = 1 if A == reg[B] else 0
    return reg


def eqri(A, B, C, reg):
    reg[C] = 1 if reg[A] == B else 0
    return reg


def eqrr(A, B, C, reg):
    reg[C] = 1 if reg[A] == reg[B] else 0
    return reg


ops = [
    addr,
    addi,
    mulr,
    muli,
    banr,
    bani,
    borr,
    bori,
    setr,
    seti,
    gtir,
    gtri,
    gtrr,
    eqir,
    eqri,
    eqrr
]

ambiguous_samples = 0
lines = get_lines()
for index, l in enumerate(lines):
    if 'Before' in l:
        before = get_numbers_from_line(l)
        op = get_numbers_from_line(lines[index + 1])
        after = get_numbers_from_line(lines[index + 2])

        similar_ops = sum([1 if f(op[1], op[2], op[3], list(before)) == after else 0 for f in ops])
        if similar_ops >= 3:
            ambiguous_samples += 1

print('The number of ambiguous samples:', ambiguous_samples)
