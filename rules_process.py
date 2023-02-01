# -*- coding: utf-8 -*-
"""
Created on Fri Jan 27 15:23:15 2023

@author: wilso
"""

import pandas as pd
import numpy as np
import os

os.chdir('C:/Users/wilso/OneDrive/Desktop')

rules = pd.read_csv('rules_repository.csv')

json_list = list(rules['parsed_rule'])

rule = pd.read_json(json_list[23])
rule_condition = rule.loc['all'].conditions[0]['params']['conditionstring']
rule_result = list(rule.loc['params'].event['action'][0])

