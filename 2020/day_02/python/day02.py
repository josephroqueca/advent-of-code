import aoc

data = aoc.load(year=2020, day=2)

class Password:
    def __init__(self, groups):
        self.range = range(int(groups[0]), int(groups[1]) + 1)
        self.letter = groups[2]
        self.value = groups[3]

    def is_valid_legacy(self):
        return self.value.count(self.letter) in self.range

    def is_valid(self):
        return (self.value[min(self.range) - 1] == self.letter) ^ (self.value[max(self.range) - 1] == self.letter)


pass_regex = r'(\d+)-(\d+) (\w): (.+)'
p1_solution = len([p for p in data.parse_lines(pass_regex, Password) if p.is_valid_legacy()])
print(p1_solution)

p2_solution = len([p for p in data.parse_lines(pass_regex, Password) if p.is_valid()])
print(p2_solution)
