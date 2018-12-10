import re
import pickle
import bz2
import lzma

import ujson
import click

from collections import defaultdict, Counter

info = '\033[1;33m[!]\033[1;m'
que = '\033[1;34m[?]\033[1;m'
bad = '\033[1;31m[-]\033[1;m'
good = '\033[1;32m[+]\033[1;m'
done = '\033[1;32m[✓]\033[1;m'
run = '\033[1;97m[~]\033[1;m'

DATEFMT = re.compile(r'RC_(\d{4}-\d{2})')


@click.command()
@click.argument('filelist', nargs=-1)
def main(filelist):

    for file in filelist:
        print('{} {}'.format(run, file))
        worker(file)


def worker(file):
    counter = defaultdict(Counter)

    if file[-3:] == 'bz2':
        opener = bz2
    elif file[-2:] == 'xz':
        opener = lzma

    with opener.open(file, 'rt') as fil:
        start = DATEFMT.search(file).group(1)
        cachefile = 'counts-{}.pickle'.format(start)

        try:
            for line in fil:
                if '[deleted]' in line:
                    continue

                parsed_line = ujson.loads(line)
                author = parsed_line['author']
                subreddit = parsed_line['subreddit']

                counter[author][subreddit] += 1

            with open(cachefile, 'wb') as cache:
                pickle.dump(counter, cache, protocol=pickle.HIGHEST_PROTOCOL)

                print('{} {}'.format(done, cachefile))

        except Exception as e:
            print('{} Error reading {}: {}'.format(bad, file, e))


if __name__ == "__main__":
    main()
