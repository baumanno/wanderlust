"""
Should be used as a CLI script, with a filename-argument
for the CSV-dump.

Connects to an sqlite-DB named "subreddits.db" in the
current directory and dumps all data in there to a CSV
file.

Second step after collecting data with "fetch_headlines.py".
"""

import sqlite3
import csv
import sys


def main(filename):
    conn = sqlite3.connect('./subreddits.db')

    with conn:
        d = conn.execute("""
            SELECT subreddit, content
            FROM headlines""")
        res = d.fetchall()

        with open(filename, 'wt') as f:
            writer = csv.writer(f)
            writer.writerow(['subreddit', 'content'])
            writer.writerows(res)

        print("Wrote {} rows".format(len(res)))


if __name__ == "__main__":
    try:
        filename = sys.argv[1]
    except IndexError:
        print('Error: please provide a filename to write to')
        import os
        sys.exit(os.errno.EINVAL)

    main(filename)
