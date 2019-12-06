#!/usr/bin/env python3

serial_number = 7511
grid_size = 300

power_levels = {}
fuel_cells = {}


def power_level(cell):
    return (((((cell[0] + 10) * cell[1] + serial_number) * (cell[0] + 10)) % 1000) // 100) - 5


def power_of_cell(cell):
    power = 0
    for xx in range(cell[2]):
        for yy in range(cell[2]):
            power += power_levels[(cell[0] - xx, cell[1] - yy)]
    return power


for z in range(1, 301):
    if fuel_cells:
        max_fuel_cell = max(fuel_cells, key=fuel_cells.get)
        print('The fuel cell with maximum power is:',
              (max_fuel_cell[0] - (z - 1), max_fuel_cell[1] - (z - 1), max_fuel_cell[2]))
    for x in range(1, grid_size + 1):
        for y in range(1, grid_size + 1):
            level = power_level((x, y))
            power_levels[(x, y)] = level
            if x > z and y > z:
                fuel_cells[(x, y, z)] = power_of_cell((x, y, z))

max_fuel_cell = max(fuel_cells, key=fuel_cells.get)
print('The fuel cell with maximum power is:', (max_fuel_cell[0] - 2, max_fuel_cell[1] - 2, max_fuel_cell[1]))
