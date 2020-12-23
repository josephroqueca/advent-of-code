import aoc

data = aoc.load(year=2020, day=23)

def get_cups(extended):
    cups = [int(c) for c in data.contents().strip()]
    if extended: cups.extend([x for x in range(max(cups), 1_000_001)])
    cups = {
        c: {'v': c, 'n': cups[i + 1] if i + 1 < len(cups) else cups[0] } \
            for i, c in enumerate(cups)
    }
    return cups

def step():
    global current, cups

    head = cups[current]
    picked_up = (cups[head['n']]['v'], cups[cups[head['n']]['n']]['v'], cups[cups[cups[head['n']]['n']]['n']]['v'])
    head['n'] = cups[cups[cups[cups[head['n']]['n']]['n']]['n']]['v']

    destination = head['v'] - 1 if head['v'] > lowest else highest
    while destination in picked_up:
        destination = destination - 1 if destination > lowest else highest

    cups[picked_up[2]]['n'] = cups[destination]['n']
    cups[destination]['n'] = picked_up[0]
    current = head['n']

# Part 1

cups = get_cups(False)
lowest = min(cups.keys())
highest = max(cups.keys())
current = next(iter(cups.keys()))

for _ in range(100):
    step()

current = cups[1]['n']
labels = []
while current != 1:
    labels.append(str(cups[current]['v']))
    current = cups[current]['n']
print(''.join(labels))

# Part 2

cups = get_cups(True)
lowest = min(cups.keys())
highest = max(cups.keys())
current = next(iter(cups.keys()))

for _ in range(10_000_000):
    step()

print(cups[1]['n'] * cups[cups[1]['n']]['n'])
