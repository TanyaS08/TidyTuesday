#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 25 17:22:39 2020

@author: tanyastrydom

CARIBOU LOCATION TRACKING
Data and README can be found at:
(https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-23)
"""

##LIBRARIES

import os
import pandas as pd
import numpy as np
from dplython import (DplyFrame, X, diamonds, select, sift, sample_n,
    sample_frac, head, arrange, mutate, group_by, summarize, DelayFunction) 

#SetWd
os.chdir('/Users/tanyastrydom/Documents/TidyTuesday/2020/CaribouTracking')

##IMPORT DATA

individuals = pd.read_csv (r'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations = pd.read_csv (r'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')


##END OF SCRIPT