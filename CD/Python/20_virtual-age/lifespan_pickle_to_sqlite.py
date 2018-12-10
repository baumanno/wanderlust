# The intention of this script is simply to move the data stored in
# lifespan.pickle into an sqlite-DB, so that calculating the average posts per
# month can be done simply in SQL, or at least in the same domain, i.e. with an
# sqlite-connection, instead of crossing over into pickle territory.
# This script should only be needed to transfer data from a pickled state into
# a database. Potentially, age.py should be updated to not use pickle at all,
# but rather write directly to DB.

import pickle
import sqlite3
import signal

from datetime import datetime

DATABASE = './posts_per_month_and_totals.db'
PICKLE = './lifespan.pickle'
CURRENT = 0
TOTAL = 0
ELAPSED = None


def handler(sig, stack):
    perc = (CURRENT / TOTAL)
    print("{} - {}% - {} - ETA in {}".format(CURRENT, perc*100, ELAPSED,
                                             ELAPSED/perc))


def main():
    global CURRENT
    global TOTAL
    global ELAPSED

    signal.signal(signal.SIGUSR1, handler)

    with open(PICKLE, 'rb') as f:
        print("Unpickling data...")
        data = pickle.load(f)
        TOTAL = len(data)

    with sqlite3.connect(DATABASE) as conn:

        print("Running SQL updates...")
        start = datetime.now()

        insert = []

        for idx, (user, active_months) in enumerate(data.items()):

            insert.append((active_months, user))
            CURRENT = idx
            ELAPSED = datetime.now() - start

        conn.executemany('''
            UPDATE OR FAIL posts
            SET months_active = ?
            WHERE username = ?
            ''', insert)

        print("Done!")
        print("Script ran for {}".format(datetime.now()-start))


if __name__ == '__main__':
    main()
