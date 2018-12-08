#!/usr/bin/env python3

import re

def get_lines(name='../input.txt'):
    with open(name) as input_file:
        return input_file.readlines()

def get_nums_by_line():
    return [[int(match) for match in re.findall(r'-?\d+', line)] for line in get_lines()]

puzzle_input = get_nums_by_line()[0]

def build_node(raw_node):
    header = {
        'children': raw_node[0],
        'metadata': raw_node[1],
    }

    node_length = 2
    children = []
    for i in range(header['children']):
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

root_node = build_node(puzzle_input)

def value_of_node(node):
    if not node['children']:
        return sum(node['metadata'])
    
    meta_cache = {}
    value = 0
    for metadata in node['metadata']:
        index = metadata - 1
        if index == -1:
            continue

        if index < node['header']['children']:
            if index not in meta_cache:
                meta_cache[index] = value_of_node(node['children'][index])
            value += meta_cache[index]
    return value

print('The value of the root node is:', value_of_node(root_node))
