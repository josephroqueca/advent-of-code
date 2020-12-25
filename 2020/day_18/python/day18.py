import aoc

data = aoc.load(year=2020, day=18)

# Part 1

def perform(first, operator, second):
    if operator == '*':
        return first * second
    elif operator == '+':
        return first + second

def resolve(it):
    result = None
    operators = []

    while val := next(it, None):
        inter = None
        if val == ' ':
            continue
        elif val in ['*', '+']:
            operators.append(val)
        elif val == '(':
            inter = resolve(it)
        elif val == ')':
            break
        else:
            inter = int(val)

        if inter:
            result = inter if not operators else perform(result, operators.pop(), inter)
    return result

p1_solution = sum([resolve(iter(x)) for x in data.lines()])
print(p1_solution)

# Part 2

def find_bracket_placement(it, inc, dec):
    depth = 0
    index = 0
    while val := next(it, None):
        index += 1
        if val.isnumeric() and depth == 0:
            break
        elif val == inc:
            depth += 1
        elif val == dec:
            depth -= 1
            if depth == 0:
                break
    return index

def add_addition_precedence(equation):
    start = 0
    while (index := equation.find('+', start)) >= 0:
        first_bracket = index - find_bracket_placement(iter(reversed(equation[:index])), ')', '(')
        second_bracket = index + find_bracket_placement(iter(equation[index + 1:]), '(', ')')
        equation = equation[:first_bracket] + '(' + equation[first_bracket:second_bracket + 1] + ')' + equation[second_bracket + 1:]
        start = index + 2
    return equation

p2_solution = sum([resolve(iter(add_addition_precedence(x))) for x in data.lines()])
print(p2_solution)
