#! /bin/sh

# Get the file size.
size=$(wc -c < "$1")
half=$(( (size + 1) / 2 ))

# Split the file.
split -b $half "$1"
