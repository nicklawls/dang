#!/usr/bin/env bash

# server programmed to read one line of pcf
# return result, then exit

# read with -r to prevent backslash escaping
# and with -d to collect all lines of example
read -r -d EOF input

output=$(echo $input | netcat localhost 5575)

# https://stackoverflow.com/a/44859148
# wtf I love bash now

if [ -z "$output" ]; then
    echo empty 1>&2;
    exit 1
elif echo "$output" | grep -Eq '#error'; then
    echo Exception 1>&2;
    echo "$output" 1>&2;
    exit 1;
elif echo "$output" | grep -Eq ':parse-error'; then
    echo Parse Error 1>&2 ;
    echo "$output" 1>&2;
    exit 2;
elif echo "$output" | grep -Eq ':type-error'; then
    echo Type Error 1>&2;
    echo "$output" 1>&2;
    exit 3;
fi

echo "Finished" 1>&2;
echo "$output" 1>&2;
echo "$output"