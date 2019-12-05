#!/usr/bin/env python3

import re

import os
script_path = os.path.dirname(os.path.realpath(__file__))
filename = '{}/../input.txt'.format(script_path)

def get_lines(name=filename):
    with open(name, 'r') as input_file:
        return input_file.readlines()

events = []
for line in get_lines():
    vals = [int(match) for match in re.findall(r'\d+', line)]
    if len(vals) == 5:
        year, month, day, hour, minute = vals
        id = -1
    else:
        year, month, day, hour, minute, id = vals
    text = line

    events.append((year, month, day, hour, minute, id, text))

guards = {}
current_guard = 0
sleeping_starts = 0
events = sorted(events)
for event in events:
    year, month, day, hour, minute, id, text = event
    if id != -1:
        current_guard = id

    if 'asleep' in text:
        sleeping_starts = minute
    elif 'wake' in text:
        if current_guard in guards:
            guards[current_guard] += minute - sleeping_starts
        else:
            guards[current_guard] = minute - sleeping_starts

minutes = {}
current_guard = -1
sleeping_starts = 0
for event in events:
    year, month, day, hour, minute, id, text = event
    if id != -1:
        current_guard = id

    if 'asleep' in text:
        sleeping_starts = minute
    elif 'wake' in text:
        for i in range(sleeping_starts, minute):
            key = (current_guard, i)
            if key in minutes:
                minutes[key] += 1
            else:
                minutes[key] = 1

highest_minute = 0
best = None
for key in minutes:
    if minutes[key] > highest_minute:
        highest_minute = minutes[key]
        best = key

print('The guard ID multiplied by the minute is', best[0] * best[1])
