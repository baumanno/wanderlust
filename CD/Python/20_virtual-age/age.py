import pickle
import glob

from collections import Counter

COUNTS = "../10_monthly-counts/"


def main():

    author_sub_monthlies = glob.glob(COUNTS + 'counts-*.pickle')
    user_counter = Counter()

    # For each month, increment the counter for a user if we see them in
    # the monthly author-to-subreddits index

    for month in sorted(author_sub_monthlies):

        print(month.split('/')[-1].split('.')[0][-7:])

        with open(month, 'rb') as f:
            author_sub_count = pickle.load(f)

        for user, _ in author_sub_count.items():
            # will increment at most by 1, because each user will appear
            # at most once in the monthly countfiles
            user_counter[user] += 1

    print('Saving counter to lifespan.pickle')

    with open('./lifespan.pickle', 'wb') as f:
        pickle.dump(user_counter, f, pickle.HIGHEST_PROTOCOL)


if __name__ == "__main__":
    main()
