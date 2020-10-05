#!/bin/env bash
# Stats 506, Fall 2020
# Group 8
# 
# This script serves as a template for Part 1 of 
# the shell scripting activity for week 1. 
#
# Author(s): Enhao Li, Yujia Wang, Moeki Kurita
# Updated: Sept 8, 2020
# 79: -------------------------------------------------------------------------

# preliminary, so you know you've run the script
message="Running week1_part1.sh ... "
echo $message

# a - download data if not present
#<2> Uncomment the lines below and fill in the file name and url. 
file="recs2015_public_v4.csv" 
url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

## if the file doesn't exist
if [ ! -f $file ]; then
  ##<3> Use wget to download the file
  wget $url
fi

# b - extract header row and output to a file with one name per line
new_file="recs_names.txt"

## delete new_file if it is already present
if [ -f $new_file ]; then
    rm $new_file 
fi

# <4> Write your one liner below.  Consider testing in multiple steps. 
head -n1 $file | tr ',' \\n > $new_file
# c - get column numbers for DOEID and the BRR weights
# as a comma separated string to pass to `cut`
# <5> write your one liner below
# grep -n -E "DOEID|BRR" $new_file | cut -f'1' -d':' | paste -s -d, -
# <6> uncomment the next three lines and copy the one liner above
cols=$(
grep -n -E "DOEID|BRR" $new_file | cut -f'1' -d':' | paste -s -d, -
)

# Uncomment the line below for testing and development:
echo $cols

# d - cut out the appropriate columns and save as recs_brrweights.csv
# <7> write your one-liner below
cut -f$cols -d',' recs2015_public_v4.csv > recs_brrweights.csv 
# $cols variable has ',' at the end of the string.
# We delete the last ',' by using '%,' in variable call

# 79: -------------------------------------------------------------------------
