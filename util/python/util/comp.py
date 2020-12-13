class Computer:
    def __init__(self, data):
        self.position = 0
        self.accumulator = 0
        self._instructions = [(i[0:3], int(i[4:])) for i in data.lines()]

    def replace(self, position, ins=None, value=None):
        self._instructions[position] = (
            ins if ins else self._instructions[position][0],
            value if value else self._instructions[position][1]
        )

    def is_finished(self):
        return self.position >= len(self._instructions)

    def instructions(self):
        return self._instructions[:]

    def step(self):
        ins, value = self._instructions[self.position]
        {
            'nop': self._nop,
            'acc': self._acc,
            'jmp': self._jmp,
        }[ins](value)

    def _nop(self, value):
        self.position += 1

    def _acc(self, value):
        self.accumulator += value
        self.position += 1

    def _jmp(self, value):
        self.position += value
