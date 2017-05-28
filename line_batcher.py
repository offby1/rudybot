from itertools import islice, chain


def batch(iterable, size):
    sourceiter = iter(iterable)
    while True:
        batchiter = islice(sourceiter, size)
        yield tuple(chain([next(batchiter)], batchiter))
