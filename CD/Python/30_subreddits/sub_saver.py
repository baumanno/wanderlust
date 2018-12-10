"""
Should be used as a CLI script, with a `limit`-parameter providing
the number of headlines to fetch.

A "headline" is the title of a submission to a Subreddit.

In one run, it fetches metadate for the subreddit, as well as the
headline content and headline-metadata.

Stores all this data in an sqlite-DB for later retrieval.
"""

import sqlite3
import pickle
import pprint

from collections import Counter

import click
import praw
import prawcore

bad = "\033[1;31m[-]\033[1;m"

errors = Counter()


def normalize(name):
    """Normalize a string so it can be used as an sqlite table name"""

    normalized = name.replace('.', '_')

    if normalized[0].isdigit():
        normalized = '-' + normalized

    return normalized


def get_subreddits():
    """Return list of subreddits from all users older than the mean.
    This data is calculated in 20_virtual-age
    """

    with open('./subreddits_unique.txt', 'r') as f:
        for i, line in enumerate(f):
            yield line.strip()


def setup_database(name='subreddits.db'):
    conn = sqlite3.connect(name)

    with conn:
        conn.execute('''
            CREATE TABLE IF NOT EXISTS subreddits_meta
            (name TEXT UNIQUE,
            description TEXT,
            audience_target TEXT,
            advertiser_category TEXT)
            ''')

        conn.execute('''
            CREATE TABLE IF NOT EXISTS headlines
            (submission_id TEXT UNIQUE,
            subreddit TEXT,
            type TEXT,
            author TEXT,
            created_utc REAL,
            num_comments INTEGER,
            score INTEGER,
            content TEXT)
            ''')

    return conn


def fetch_sub(reddit, name):

    # lazy object, still needs to be fetched
    sub = reddit.subreddit(name)

    # accessing an attribute fetches the full model
    title = sub.title

    print("[{}] {}".format(name, title))

    return sub


@click.command()
@click.argument('limit', type=int)
def main(limit):
    reddit = praw.Reddit('usercrawler')

    # set up database
    conn = setup_database()

    # fetch each subreddit and save metadata
    for sub in get_subreddits():

        with conn:
            test = conn.execute('''
                    SELECT *
                    FROM subreddits_meta
                    WHERE name=?''', (sub,))
            if len(test.fetchall()) > 0:
                print("Heya, {} already exists, skipping it".format(sub))
                continue

        try:
            sub_fetched = fetch_sub(reddit, sub)

        except (prawcore.exceptions.NotFound,
                prawcore.exceptions.Forbidden) as e:

            print("{} {} accessing {}".format(bad, e, sub))

            if e.response.status_code is 403:
                errors["Forbidden"] += 1
            elif e.response.status_code is 404:
                errors["Not Found"] += 1
            else:
                errors[e.response.reason] += 1

            continue

        with conn:
            try:
                conn.execute('''
                    INSERT INTO subreddits_meta
                    VALUES (?, ?, ?, ?)''',
                             (sub,
                              sub_fetched.description,
                              sub_fetched.audience_target,
                              sub_fetched.advertiser_category))

            except sqlite3.IntegrityError as e:
                print("{} exists!".format(sub))

        headlines = sub_fetched.top(limit=limit)
        insert_queries = []

        try:
            for headline in headlines:
                if headline.author is None:
                    author = ''
                else:
                    author = headline.author.name

                insert_queries.append(
                    (headline.id, sub, 'top', author, headline.created_utc,
                     headline.num_comments, headline.score, headline.title)
                )
        except prawcore.exceptions.Forbidden as e:
            print("{} {} accessing headlines for {}".format(bad, e, sub))
            continue

        with conn:
            conn.executemany('''
                INSERT OR IGNORE INTO headlines VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                ''', insert_queries)

    pprint.pprint(errors)

    with open('errors.pickle', 'w') as f:
        pickle.dump(errors, f)


if __name__ == "__main__":
    main()
