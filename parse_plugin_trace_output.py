#! /usr/bin/env python

# Parses the output from a traced run of the ConCat plugin.
#
# Original author: David Banas <capn.freako@gmail.com>
# Original date:   April 12, 2018
#
# Copyright (c) 2018 David Banas; all rights reserved World wide.
#
# Example usage:
#   stack build :notMNIST_Graph 1>stack_build_notMNIST_Graph.out 2>stack_build_notMNIST_Graph.err
#   ./parse_plugin_trace_output.py <stack_build_notMNIST_Graph.err

import sys
# from future_builtins import map

doing_lines = {}
go_lines = {}
bad_dicts = {}
state = 0
lines = 0
for line in map(str.rstrip, sys.stdin):
    lines += 1
    if(state == 0):
        # Doing line?
        if(line.startswith("Doing ")):
            if(line in doing_lines):
                doing_lines[line] += 1
            else:
                doing_lines[line] = 1
        # Go line?
        if(line.startswith("go ")):
            if(line in go_lines):
                go_lines[line] += 1
            else:
                go_lines[line] = 1
        # Couldn't build dictionary line?
        if(line.startswith("Couldn't build dictionary for")):
            state = 1
    elif(state == 1):
        if(line not in bad_dicts):
            bad_dicts[line] = {}
        curr_dict = line
        state = 2
    elif(state == 2):
        if(line.startswith("  free id types: ")):
            if(line in bad_dicts[curr_dict]):
                bad_dicts[curr_dict][line] += 1
            else:
                bad_dicts[curr_dict][line] = 1
            state = 0
print("Processed", lines, "lines.")
print("")
print("Doing lines:")
print("___________")
for key,val in list(doing_lines.items()):
    print(key, ": ", val)
print("")
print("Go lines:")
print("________")
for key,val in list(go_lines.items()):
    print(key, ": ", val)
print("")
print("Bad dictionaries:")
print("________________")
for key,val in list(bad_dicts.items()):
    print(key, ":")
    for x,n in list(val.items()):
        print("\t", x, ": ", n)

