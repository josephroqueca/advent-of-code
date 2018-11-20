##############################
#                            #
#        Instructions        #
#                            #
##############################

# To run, use the following command:
# $ python lights.py <input_file>
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

# Initialize all lights to off in 1000x1000 matrix
value_to_offset = 0
total_brightness = 0
light_matrix = [[0 for i in range(1000)] for j in range(1000)]

# Regular expression to find values of coordinates
coordinates_regex = re.compile('\d+')

# For every line in the input
for line in puzzle_input:

    # Set lights to on, off, or toggle them
    if 'on' in line: value_to_offset = 1
    elif 'off' in line: value_to_offset = -1
    else: value_to_offset = 2

    # Getting coordinates of lights to be adjusted
    lights_to_change = re.findall(coordinates_regex, line)
    lights_to_change = [int(i) for i in lights_to_change[:4]]

    # For each light in the range, adjust its value
    for x in range(lights_to_change[0], lights_to_change[2] + 1):
        for y in range(lights_to_change[1], lights_to_change[3] + 1):
            light_matrix[x][y] = max(light_matrix[x][y] + value_to_offset, 0)

# Count the lights that remain on
for x in range(1000):
    for y in range(1000):
        total_brightness += light_matrix[x][y]

# Print the total brightness of the lights
print('The total brightness is', total_brightness)
