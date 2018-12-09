#!/bin/bash

language=$1
year=$2
day=$3

if [ ${#day} -le 1 ]; then
    day=0$day
fi

extension="py"
case $language in
    "python") ;;
    "haskell") extension="hs" ;;
    "ruby") extension="rb" ;;
    *) echo "$language is not a supported language."; exit 1 ;;
esac

working_dir=$year/day_$day/$language/
mkdir -p $working_dir
cp ./util/starter.$extension $working_dir/p1.$extension
cp ./util/starter.$extension $working_dir/p2.$extension
touch $working_dir/../input.txt
touch $working_dir/../test.txt

