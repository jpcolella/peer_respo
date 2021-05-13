#! /usr/bin/env python 3

import glob,os
import sys
import pandas as pd
import re
import csv
import numpy as np
from itertools import combinations
from datetime import datetime
from datetime import timedelta
        
#==================================
# RUN SCRIPT
#==================================
all_dat = "analysis_data_final_2July2020.csv"
output = "analysis_data_final_2July2020_DSTcorr.csv"

with open(all_dat, 'r') as infile:
    with open(output,'w') as outcsv:
        dat = pd.read_csv(infile, header=0, index_col=1, parse_dates=True, squeeze=True)

        #Select all rows with animal data (e.g., NOT baseline chamber data)
        DST_Str = '02:00:00 3/8/20'
        DST = datetime.strptime(DST_Str, '%H:%M:%S %m/%d/%y')

        # For each time beyond DST, subtract an hour (b/c it's "Spring ahead"
        for each_time in dat.index:
            if each_time >= DST:
                each_time = each_time - timedelta(hours=1) 
                
        #Write to output csv file    
        dat.to_csv(outcsv)
  
