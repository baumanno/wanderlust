import pickle
import sqlite3
import os
import sys
from multiprocessing import Process, JoinableQueue, Queue

from glob import glob
from collections import Counter

# constants
PATH = '../10_monthly-counts/'
FILES = sorted(glob(PATH + 'counts*.pickle'))
DBFILE = 'posts_per_month_and_totals.db'
PROCS = 1


def worker(task_queue, result_queue):
    for file in iter(task_queue.get, 'STOP'):
        counter = process(file)
        task_queue.task_done()
        result_queue.put(counter)


def process(file):
    print(file, os.getpid())
    counter = Counter()

    # load data from monthly counts to summarize posts per month
    with open(file, 'rb') as f:
        data = pickle.load(f)

    for user, counts in data.items():
        total = 0

        # each item is a subreddit:count pair; iterating over subs and
        # summing counts yoields total for that month, as we are using
        # monthly count-aggregations (counts-YYYY-MM.pickle)
        for sub, count in counts.items():
            total += count

        counter[user] = total

    return counter


def main():

    result_queue = Queue()
    task_queue = JoinableQueue()

    for f in FILES:
        task_queue.put(f)

    for _ in range(PROCS):
        task_queue.put('STOP')

    for p in range(PROCS):
        Process(target=worker, args=(task_queue, result_queue)).start()

    for _ in range(len(FILES)):
        counter = result_queue.get()
        save(counter)


def save(counter):
    print("Picked up a length {} counter".format(len(counter)))

    with sqlite3.connect(DBFILE) as conn:
        conn.row_factory = sqlite3.Row

        for user, count in counter.items():
            cursor = conn.execute("""
                    SELECT total_count
                    FROM posts
                    WHERE username=?""", (user,))

            result = cursor.fetchone()

            if result is None:
                result = (0,)

            conn.execute("""
                INSERT OR REPLACE INTO posts (username, total_count)
                VALUES (?, ?)""", (user, result[0]+count))


def setup():
    with sqlite3.connect(DBFILE) as conn:
        conn.execute("DROP TABLE posts")
        conn.execute('''
            CREATE TABLE posts
            (username TEXT UNIQUE,
            total_count INTEGER)''')

    print("Created database")


if __name__ == '__main__':
    if sys.argv[1] == 'setup':
        setup()

    main()
