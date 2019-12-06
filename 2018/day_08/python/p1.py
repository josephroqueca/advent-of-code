#!/usr/bin/env python3

import re

import os
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__))
FILENAME = '{}/../input.txt'.format(SCRIPT_PATH)


def get_lines(name=FILENAME):
    with open(name) as input_file:
        return input_file.readlines()


def get_nums_by_line():
    return [[int(match) for match in re.findall(r'-?\d+', line)] for line in get_lines()]


PUZZLE_INPUT = get_nums_by_line()[0]


def build_node(raw_node):
    header = {
        'children': raw_node[0],
        'metadata': raw_node[1],
    }

    node_length = 2
    children = []
    for _ in range(header['children']):
        child = build_node(raw_node[node_length:-header['metadata']])
        node_length += child['length']
        children.append(child)

    metadata = raw_node[node_length:node_length + header['metadata']]
    node_length += header['metadata']

    return {
        'header': header,
        'length': node_length,
        'children': children,
        'metadata': metadata,
    }


root_node = build_node(PUZZLE_INPUT)


def sum_metadata(node):
    total = sum(node['metadata'])
    for child in node['children']:
        total += sum_metadata(child)
    return total


sum_of_metadata = sum_metadata(root_node)
print('The sum of all the node metadata is:', sum_of_metadata)
