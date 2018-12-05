# Reading challenge input

def get_file(name='../input.txt'):
    with open(name) as input_file:
        return input_file.read().strip()

puzzle = get_file()

def next_index(after, removed):
    n = after + 1
    while n in removed:
        n += 1
    return n

def prev_index(before, removed):
    p = before - 1
    while p in removed:
        p -= 1
    if p < 0:
        p = next_index(p, removed)
    return p

cur_index = 0
removed_elements = set()
lower_puzzle = puzzle.lower()
while cur_index < len(puzzle) - 1:
    compare_index = next_index(cur_index, removed_elements)
    if compare_index >= len(puzzle):
        break
    
    if lower_puzzle[cur_index] == lower_puzzle[compare_index] and puzzle[cur_index] != puzzle[compare_index]:
        removed_elements.add(cur_index)
        removed_elements.add(compare_index)
        cur_index = prev_index(cur_index, removed_elements)
        continue

    cur_index = next_index(cur_index, removed_elements)

puzzle_size = len(puzzle) - len(removed_elements)
print(puzzle_size, 'units remain after fully reacting the polymer')
