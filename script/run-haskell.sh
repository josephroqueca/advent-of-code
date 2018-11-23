#!/bin/sh

# Check the length of the day input
# Prepend a 0 if it's less than 10 i.e 4 -> 04, 11 -> 11
day=$2
if [ ${#2} -le 1 ]; then
    day=0$2
fi

# Change directory, build haskell script, run haskell script
cd $1/day_$day/
stack ghc -- $3.hs -o $3.hsx
./$3.hsx
