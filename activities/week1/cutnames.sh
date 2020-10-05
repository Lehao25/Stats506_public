#!/bin/env bash
# Stats 506, Fall 2020, Group 8
# 
# For the second argument PATTERN,
# this script interprets PATTERN as an extended regular expression  (ERE)
#
# Author(s): Enhao Li, Yujia Wang, Moeki Kurita
# Updated: Sept 8, 2020
# 79: -------------------------------------------------------------------------

# Convert arguments into variables
# See part2 step4
file=$1
pattern=$2

# get column numbers for pattern as a comma separated string
cols=$(
head -n1 $file | tr ',' \\n | grep -n -E $pattern | cut -f'1' -d':' | paste -s -d, -
)

# check output
# echo $cols

# extract colums from the given data
cut -f$cols -d',' $file

# 79: -------------------------------------------------------------------------
