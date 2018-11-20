##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python bitwise_extended.py <input_file>
# where <input_file> is the filename with the question's input

import sys
import re

# Check to make sure correct number of arguments supplied
if (len(sys.argv) != 2):
    print('Invalid number of arguments!')
    sys.exit()

# Read the input from the file provided as argument
input_file = open(sys.argv[1])
puzzle_input = input_file.readlines()
input_file.close()

# Initialize the empty, incomplete circuit
circuit = {}

# Resets the state of the circuit
def reset():
    global circuit
    circuit = {}

# Takes a signal input, either a gate or numeric value, returns a numeric value for the signal
def parse_signal(signal):

    # Signal is determined by an AND gate
    if 'AND' in signal:
        # Get the wire identifiers
        wires = re.findall('[a-z]+|\d+', signal)

        # Get the signals of the two wires that make up the AND gate
        first_wire_signal = get_signal(wires[0])
        second_wire_signal = get_signal(wires[1])

        # Return bitwise AND of the two wire signals
        if first_wire_signal is None or second_wire_signal is None: return None
        else: return first_wire_signal & second_wire_signal

    # Signal is determined by a LSHIFT gate
    elif 'L' in signal:
        # Get the wire identifier
        wires = re.findall('([a-z]+|\d+)', signal)

        # Get the signal of the wire and the value to left shift it by
        wire_signal = get_signal(wires[0])
        shift = int(wires[1])

        # Return bitwise left shift of the wire signal by shift
        if wire_signal is None: return None
        else: return wire_signal << shift

    # Signal is determined by a RSHIFT gate
    elif 'RS' in signal:
        # Get the wire identifier
        wires = re.findall('([a-z]+|\d+)', signal)

        # Get the signal of the wire and the value to right shift it by
        wire_signal = get_signal(wires[0])
        shift = int(wires[1])

        # Return bitwise right shift of the wire signal by shift
        if wire_signal is None: return None
        else: return wire_signal >> shift

    # Signal is determined by an OR gate
    elif 'OR' in signal:
        # Get the wire identifiers
        wires = re.findall('[a-z]+|\d+', signal)

        # Get the signals of the two wires that make up the OR gate
        first_wire_signal = get_signal(wires[0])
        second_wire_signal = get_signal(wires[1])

        # Return bitwise OR of the two wire signals
        if first_wire_signal is None or second_wire_signal is None: return None
        else: return first_wire_signal | second_wire_signal

    # Signal is determined by a NOT gate
    elif 'NOT' in signal:
        # Get the wire identifier
        wires = re.findall('[a-z]+|\d+', signal)

        # Get the signal of the wire
        wire_signal = get_signal(wires[0])

        # Return bitwise NOT of the wire signal
        if wire_signal is None: return None
        else: return ~wire_signal

    # Signal is a raw value
    else:
        # Get the wire identifier
        wires = re.findall('[a-z]+|\d+', signal)

        # Get the signal of the wire
        wire_signal = get_signal(wires[0])

        # Return the wire signal
        if wire_signal is None: return None
        else: return wire_signal

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
        else:
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
for line in puzzle_input:
    # Get the wire and signal and add them to the dictionary
    wire, signal = extract_wire_and_signal(line)
    try:
        circuit[wire] = int(signal)
    except ValueError:
        circuit[wire] = signal

# Get the signal of a, then reset the circuit to run again
signal_of_a = get_signal('a')
reset()

# Same as above, but this time replace the wire of b with the literal signal of a
for line in puzzle_input:
    wire, signal = extract_wire_and_signal(line)
    if wire == 'b':
        circuit[wire] = signal_of_a
    else:
        try:
            circuit[wire] = int(signal)
        except ValueError:
            circuit[wire] = signal

# Get the new signal of a
signal_of_a = get_signal('a')

# Print the signal
print('The new signal of \'a\' is:', signal_of_a)
