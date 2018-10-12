#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

DEBUG=TRUE
BYPASS=TRUE

sleep $@;

for i in $(seq 2007 2018); do
    for j in $(seq 1 12); do
        ./3_alters_top_topics.R monocasa $i $j $BYPASS $DEBUG;
        ./3_alters_top_topics.R formido $i $j $BYPASS $DEBUG;
        ./3_alters_top_topics.R cavedave $i $j $BYPASS $DEBUG;
        ./3_alters_top_topics.R IronWolve $i $j $BYPASS $DEBUG;
    done;
done;
notify-send "fetched alters topics"
