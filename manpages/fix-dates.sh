#!/bin/sh
#
# Run inside a git repository to update the man page dates.

for F in $(ls *.[1-8])
do
   tstamp=$(git log -1 --format=%ct ${F} | xargs -I {} date -r {} +"%B %d, %Y")
   echo "${F}: ${tstamp}"
   sed -i '' -e "s/.Dd .*$/.Dd ${tstamp}/" "${F}"
done
