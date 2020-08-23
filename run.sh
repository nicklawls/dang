#!/usr/bin/env bash

# server programmed to read one line of pcf
# return result, then exit

# read with -r to prevent backslash escaping
read -r input

echo in: $input 1>&2

output=$(echo $input | netcat localhost 5575)

echo out: "$output" 1>&2

# https://stackoverflow.com/a/44859148
# wtf I love bash now

if [ -z "$output" ]; then
    echo empty 1>&2;
    exit 1
elif echo "$output" | grep -Eq '^\{:parse-error.+\}$'; then
    echo PARSE 1>&2 ;
    exit 2;
elif echo "$output" | grep -Eq '^:type-error$'; then
    echo typeeee 1>&2;
    exit 3;
fi

echo "$output"