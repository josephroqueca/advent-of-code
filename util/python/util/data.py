import math
import re

class _Data:

    def __init__(self, contents):
        self._contents = contents

    # Raw string contents
    def contents(self):
        return self._contents[:]

    # List of lines in the file
    def lines(self):
        return self._contents.splitlines()

    # List of numbers in the file, when they are separated 1 number per line
    def numbers(self):
        return [int(re.search(r'-?\d+', line).group(0)) for line in self.lines()]

    # List of numbers in the file, with multiple numbers per line
    def numbers_by_line(self):
        return [self._parse_number_line(line) for line in self.lines()]

    # Parse as a table. `data` should be a list of 'w' or 'd'
    # 'd' columns are parsed as integers, 'w' columns are strings
    # `sep` can be provided to split the columns on a different value
    def table(self, data, sep=','):
        return [[
            int(col[1]) if col[0] == 'd' else col[1]
                for col in zip(data, line.split(sep))
        ] for line in self.lines()]

    # List of lines in the file, parsed by a regex.
    # The groups from the regex match are passed to `container`, which returns a list by default
    # But could be an initializer or namedtuple, etc.
    # SEE 2020/day_02
    def parse_lines(self, regex, container=list):
        return [self._parse_regex(regex, line, container) for line in self.lines()]

    # Define how multiple chunks of the content will be parsed. A chunk will be used until a line
    # fails to parse, or the max count is reached
    # chunks:
    # { 'type': TYPE, 'count': COUNT, 'value': VALUE }
    # where:
    #   - TYPE is 'regex', 'numbers', or 'drop'
    #     'regex' chunks take the regex in VALUE and parse the line
    #     'numbers' chunks returns all of the numeric values in the line
    #     'drop' chunks drop the lines from the input.
    #   - VALUE is a regex
    #   - COUNT is the maximum number of times to use a chunk
    # SEE 2020/day_16
    def parse_by_chunks(self, chunks):
        output = []
        lines = self.lines()
        for chunk in chunks:
            chunk_count = 0
            chunk_output = []
            chunk_max = chunk['count'] if 'count' in chunk else math.inf
            while lines and chunk_count < chunk_max:
                l = lines[0]
                if chunk['type'] == 'regex':
                    parsed = self._parse_regex(chunk['value'], l)
                    if not parsed: break
                    chunk_output.append(parsed)
                elif chunk['type'] == 'numbers':
                    numbers = self._parse_number_line(l)
                    if not numbers: break
                    chunk_output.append(numbers)
                elif chunk['type'] == 'string':
                    chunk_output.append(l)
                elif chunk['type'] == 'drop':
                    pass
                lines.pop(0)
                chunk_count += 1

            if chunk['type'] != 'drop':
                output.append(chunk_output)
        return output

    def _parse_regex(self, regex, line, container=list):
        parsed = None
        try:
            parsed = container(re.search(regex, line).groups())
        except:
            pass
        return parsed

    def _parse_number_line(self, line):
        return [int(match) for match in re.findall(r'-?\d+', line)]
