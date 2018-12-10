#!/usr/bin/env zsh

OUTPUT="subreddits.csv"
OUTPUT_UNIQ="subreddits_unique.txt"

python collect_subreddits.py > $OUTPUT

echo -ne "Stripping...\n"
cut -d',' -f1 $OUTPUT > cache

echo -ne "Sorting...\n"
sort --parallel=4 -T . cache > cache.sorted

echo -ne "Uniq'ing...\n"
uniq cache.sorted > $OUTPUT_UNIQ

echo -ne "`wc -l $OUTPUT_UNIQ` unique subreddits\n"

rm -f cache

