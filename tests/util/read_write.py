#!/bin/python3
"""
Utility functions to read samuca outputs
"""

import shutil
import pandas as pd
import datetime
from pathlib import Path

def str2bool(x: str)->bool:
    '''convert str to bool'''
    return x.lower() in ['true','t','yes','y','1']

def check_type(variable):
    if isinstance(variable, int):
        return "Integer"
    elif isinstance(variable, float):
        return "Float"
    elif isinstance(variable, str):
        return "String"
    else:
        return "Unknown type"

def ReadFile(fn: str)->list:
        '''ReadLines of a file'''                
        with open(fn) as file:
            lines = [line.rstrip() for line in file]
        return lines

def WriteFile(fn: str, lines: list)->None:
    '''Write the file'''            
    with open(fn, 'w') as f:
        for line in lines:
            f.write(f"{line}\n")

def grep_index(file_lines: list, var_group: str, var_name: str)->tuple:
    '''Returns a 3-len tuple: (1) index of first ocurrence of variable name in file_lines, (2 and 3) string start and end index within the line'''
    in_group = False    
    for line in file_lines:
        if in_group and ('*' in line[0]):
            break
        if (f'*{var_group}' in line[0:len(var_group)+2]): 
            in_group=True
        if in_group and (var_name in line):
            idx_start = line.find(var_name)
            idx_end = idx_start+len(var_name)
            return (file_lines.index(line), idx_start, idx_end)
    if not in_group:
        print(f'Variable group=[{var_group}] not found in {file_lines}')
    else:
        print(f'Variable name="{var_name}" not found in group=[{var_group}] of {file_lines}')
    return None

def replace_substring_rjust(text: str, start: int, end: int, replacement: str, revert_rjust = True)->str:
    """Replace substring within a start-end interval rjusted"""     
    if len(text) < end:
         #--- add trailing whitespaces
         text = text+" "*(end - len(text))    
    if len(replacement) > (end-start):
         #--- trim
         replacement = replacement[0:(end-start)]
    else:
         if revert_rjust:            
            #--- ljust
            replacement = replacement.ljust(end-start)
         else:             
            #--- rjust
            replacement = replacement.rjust(end-start)    
    if start < 0 or end > len(text) or start >= end:
        raise ValueError("Invalid start or end index")
    return text[:start] + replacement + text[end:]

def update_file_lines(file_lines: list, var_group: str, var_name: str, var_value: str, file_lines_name= 'file_lines')->list:
    '''Try to find and replace the value a given variable in a XPN cfg file (file_lines)'''
    try:
        file_lines[grep_index(file_lines,var_group,var_name)] = f'{var_name}= {var_value}'
    except:
        print(f'Variable="{var_name}" was not updated in file: {file_lines_name}')
    return file_lines
