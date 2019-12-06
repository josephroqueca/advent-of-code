#!/usr/bin/env python3

import re
import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)

# Read the challenge input
with open(FILENAME, 'r') as input_file:
    PUZZLE_INPUT = input_file.readlines()


# Initialize the empty, incomplete circuit
circuit = {}


def reset():
    # Resets the state of the circuit
    global circuit
    circuit = {}


def parse_signal(signal):
    # Takes a signal input, either a gate or numeric value, returns a numeric value for the signal
    # Signal is determined by an AND gate
    if 'AND' in signal:
        # Get the wire identifiers
        wires = re.findall(r'[a-z]+|\d+', signal)

        # Get the signals of the two wires that make up the AND gate
        first_wire_signal = get_signal(wires[0])
        second_wire_signal = get_signal(wires[1])

        # Return bitwise AND of the two wire signals
        if first_wire_signal is None or second_wire_signal is None:
            return None
        return first_wire_signal & second_wire_signal

    # Signal is determined by a LSHIFT gate
    if 'L' in signal:
        # Get the wire identifier
        wires = re.findall(r'([a-z]+|\d+)', signal)

        # Get the signal of the wire and the value to left shift it by
        wire_signal = get_signal(wires[0])
        shift = int(wires[1])

        # Return bitwise left shift of the wire signal by shift
        if wire_signal is None:
            return None
        return wire_signal << shift

    # Signal is determined by a RSHIFT gate
    if 'RS' in signal:
        # Get the wire identifier
        wires = re.findall(r'([a-z]+|\d+)', signal)

        # Get the signal of the wire and the value to right shift it by
        wire_signal = get_signal(wires[0])
        shift = int(wires[1])

        # Return bitwise right shift of the wire signal by shift
        if wire_signal is None:
            return None
        return wire_signal >> shift

    # Signal is determined by an OR gate
    if 'OR' in signal:
        # Get the wire identifiers
        wires = re.findall(r'[a-z]+|\d+', signal)

        # Get the signals of the two wires that make up the OR gate
        first_wire_signal = get_signal(wires[0])
        second_wire_signal = get_signal(wires[1])

        # Return bitwise OR of the two wire signals
        if first_wire_signal is None or second_wire_signal is None:
            return None
        return first_wire_signal | second_wire_signal

    # Signal is determined by a NOT gate
    if 'NOT' in signal:
        # Get the wire identifier
        wires = re.findall(r'[a-z]+|\d+', signal)

        # Get the signal of the wire
        wire_signal = get_signal(wires[0])

        # Return bitwise NOT of the wire signal
        if wire_signal is None:
            return None
        return ~(wire_signal or 0)

    # Get the wire identifier
    wires = re.findall(r'[a-z]+|\d+', signal)

    # Get the signal of the wire
    wire_signal = get_signal(wires[0])

    # Return the wire signal
    if wire_signal is None:
        return None
    return wire_signal

# Gets the signal of a wire from the dictionary, or computes it if necessary
# Computed values are then stored in the dictionary for easy retrieval later


def get_signal(wire):
    try:
        # First, check if the 'wire' is simply a literal
        return int(wire)
    except ValueError:
        # If wire is not a literal int, get the signal of the wire from circuit
        signal = circuit[wire]

        # If the signal is not an int, it needs to be parsed
        if not isinstance(signal, int):
            signal = parse_signal(signal)

        # If the signal is positive, return it
        if signal >= 0:
            circuit[wire] = signal
            return signal

        # If it is negative, it overflows, with size 2^16
        signal += 2 ** 16
        circuit[wire] = signal
        return signal

# Gets the wire and its signal definition from a string


def extract_wire_and_signal(string):
    arrow = string.find('->')
    wire = string[arrow + 3:].strip()
    signal = string[:arrow - 1].strip()
    return (wire, signal)


# For each line in the input
for line in PUZZLE_INPUT:
    # Get the wire and signal and add them to the dictionary
    w, s = extract_wire_and_signal(line)
    try:
        circuit[w] = int(s)
    except ValueError:
        circuit[w] = s

# Get the signal of a, then reset the circuit to run again
signal_of_a = get_signal('a')
reset()

# Same as above, but this time replace the wire of b with the literal signal of a
for line in PUZZLE_INPUT:
    w, s = extract_wire_and_signal(line)
    if w == 'b':
        circuit[w] = signal_of_a
    else:
        try:
            circuit[w] = int(s)
        except ValueError:
            circuit[w] = s

# Get the new signal of a
signal_of_a = get_signal('a')

# Print the signal
print('The new signal of \'a\' is:', signal_of_a)
