# Reading challenge input

def get_file(name='../input.txt'):
    with open(name) as input_file:
        return input_file.read()

original_puzzle = get_file()

available_units = set()
for c in original_puzzle.lower():
    available_units.add(c)

smallest_size = len(original_puzzle)
for c in available_units:
    print(c)
    puzzle = original_puzzle.replace(c.upper(), '').replace(c, '')

    repeat = True
    while repeat:
        repeat = False
        index = 0
        while index < len(puzzle) - 1:
            compare_index = index + 1

            if puzzle[index].lower() == puzzle[compare_index].lower():
                if puzzle[index] != puzzle[compare_index]:
                    puzzle = puzzle.replace(puzzle[index] + puzzle[compare_index], '')
                    repeat = True
            index += 1

    if len(puzzle) < smallest_size:
        smallest_size = len(puzzle)

print(smallest_size)

