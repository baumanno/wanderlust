import glob
import pickle
import os

os.chdir(os.path.expanduser("~/Masterarbeit/code/analysis"))


def main():

    # pickle is a Series: username => age (in months)
    with open('20_virtual-age/users_larger_than_mean_age.pickle', 'rb') as f:
        ages = pickle.load(f)

    monthly_indexes = sorted(glob.glob('10_monthly-counts/counts*.pickle'))

    for index in monthly_indexes:
        with open(index, 'rb') as f:
            month = pickle.load(f)

        for user in ages.index:
            if month[user]:
                for i, j in month[user].items():
                    print("{},{}".format(i, j))


if __name__ == "__main__":
    main()
