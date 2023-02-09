# -*- coding: utf-8 -*-
"""
Created on Fri Jan 27 15:23:15 2023

@author: wilso
"""

import pandas as pd
import numpy as np
import os
import re

os.chdir('C:/Users/Lee/Desktop/Work/Network_process')

rules = pd.read_csv('rules_repository.csv')

json_list = list(rules['parsed_rule'])


STATES = [ "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
          "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",     
          "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",     
          "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",     
          "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY","PR"]

SASFUNC = ['=','==','+','-','*','/','>','<','>=','<=','~=','&','|','and','or',
           'sum','avg','min','max','count','mean','median','mode','round','abs',
           'int','if','then','else','substr','in','Y','N','agg','concat',
           'getTime','not']

SYMBOLS = '{}()[].,:;+-*/&|<>=~$1234567890'

input_list = []
for i in json_list:
    rule = pd.read_json(i)
    rule_condition = rule.loc['all'].conditions[0]['params']['conditionstring']
    tokenized = re.findall(r"(\b\w*[\.]?\w+\b|[\(\)\+\*\-\/])", rule_condition)
    new = [tokenized for tokenized in tokenized if not (tokenized in SYMBOLS or tokenized[0].isdigit() or tokenized.isdigit() or tokenized in SASFUNC or tokenized in STATES)]
    input_list.append(new)

output_list = []
for i in json_list:
        result_val = []
        rule = pd.read_json(i)
        rule_result = rule.loc['params'].event['action']
        for j in rule_result:
            key_val = list(j.keys())
            result_val.extend(key_val)
            result_val = [x.strip(' ') for x in result_val]
            result_val = [x.strip('\n') for x in result_val]
        output_list.append(list(result_val))
        
for i,j in enumerate(output_list):
    if j == []:
        output_list[i] = ["BLANK"]     
        
for i,j in enumerate(input_list):
    if j == []:
        input_list[i] = ["BLANK"]     
                
def find_index(list1, list2):
    result = []
    result2 = []
    for i, sub_list1 in enumerate(list1):
        for j, sub_list2 in enumerate(list2):
            if all(x in sub_list2 for x in sub_list1):
                result.append((i))
                result2.append((j))
    return result, result2


print(find_index(output_list, input_list))

output_idx, input_idx = find_index(output_list, input_list)

        
        

        