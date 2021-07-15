#!/usr/bin/env python

import argparse
import os
from pathlib import Path
import pandas as pd
import numpy as np
from sklearn.cluster import KMeans
import itertools
import math


# Parse command-line arguments
parser = argparse.ArgumentParser(description="K-means clustering",
                                 allow_abbrev=False,
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("-i","--tsv", action="store", type=Path, required=True, help="Input file.")
parser.add_argument("-d","--dtype", action="store", default=None, choices=["int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "float16", "float32", "float64"], help="Data type to use.")
parser.add_argument("-c","--cpu", action="store", type=int, default=1, help="Number of threads.")
parser.add_argument("-b","--mini_batch_kmeans", action="store_true", default=False, help="Use the MiniBatchKMeans algorithm, instead of the default KMeans.")
parser.add_argument("-k","--n_clusters", action="store", type=int, default=100, help="Number of Ks.")
parser.add_argument("-n","--n_starts", action="store", type=int, default=10, help="Number of independent starts.")
parser.add_argument("-t","--transpose", action="store_true", default=False, help="Transpose matrix?")
parser.add_argument("-s","--seed", action="store", type=int, default=12345678, help="RNG seed.")
parser.add_argument("-v","--verbose", action="store", type=int, default=1, help="Verbose level.")
parser.add_argument("-o","--out_prefix", action="store", type=str, default="output", help="Output prefix.")
args = parser.parse_args()


# Set number of threads to use
os.environ["OMP_NUM_THREADS"] = str(args.cpu)


# Read TSV, and treat all negative values as zero
if args.verbose > 0:
    print("====> Reading TSV")
col_names = pd.read_csv(args.tsv, sep="\t", nrows=0).columns
dtypes = dict(zip(col_names,np.repeat(args.dtype, len(col_names))))
df = pd.read_csv(args.tsv, sep="\t", dtype=dtypes)
if args.verbose > 4:
    print(df.dtypes)
    print(df.memory_usage(deep=True).sum() / (1024**2)) # Memory usage in Mb


# Transpose matrix
if args.transpose:
    if args.verbose > 0:
        print("====> Transposing matrix")
    df = df.transpose()


# Check number unique rows
if args.verbose > 0:
    print("====> Checking unique rows")
df_uniq = df.drop_duplicates()
if df_uniq.shape[0] <= args.n_clusters:
    print("WARN: number of clusters and independent starts reduced, due to presence of duplicated rows.")
    args.n_clusters = df_uniq.shape[0]
    args.n_starts = 1


if args.verbose > 0:
    print(f"Number of columns: {df.shape[1]}")
    print(f"Number of rows: {df.shape[0]}")
    print(f"Number of unique rows: {df_uniq.shape[0]}")
    print(f"Number of Ks: {args.n_clusters}")
    print(f"Number of independent starts: {args.n_starts}")


if args.mini_batch_kmeans:
    if args.verbose > 0:
        print("====> Performing Mini Batch K-Means clustering")
    kmeans = MiniBatchKMeans(n_clusters = args.n_clusters, 
                             max_iter = 300, 
                             n_init = args.n_starts, 
                             random_state = args.seed, 
                             batch_size = 100,
                             verbose = 0).fit(df)
else:
    if args.verbose > 0:
        print("====> Performing K-Means clustering")
    kmeans = KMeans(n_clusters = args.n_clusters, 
                    max_iter = 300, 
                    n_init = args.n_starts, 
                    random_state = args.seed,
                    verbose = 0).fit(df)


#kmeans.get_params()
print(f"Clustering finished after {kmeans.n_iter_} iterations and with an inertia of {kmeans.inertia_}.")
if len(df.index) != len(kmeans.labels_):
    raise ValueError("Invalid values")


# Cluster labels
if args.verbose > 0:
    print("====> Saving cluster labels")
with open(f"{args.out_prefix}.km_clusters.tsv", "w") as fp:
    fp.write("Cluster_ID\n")
    fp.write("\n".join("{}\t{}".format(x[0],x[1]) for x in list(zip(df.index,kmeans.labels_))))


# K-M clustering
if args.verbose > 0:
    print("====> Saving K-M clustering")
cluster_sizes = list(zip(np.unique(kmeans.labels_), np.bincount(kmeans.labels_)))
km = pd.DataFrame(kmeans.cluster_centers_, columns = df.columns, index = [f"Cluster: {c} Size: {s}" for c,s in cluster_sizes])
km.to_csv(f"{args.out_prefix}.km.tsv", sep="\t")


# K-M weighted clustering
#if args.verbose > 0:
#    print("====> Saving K-M weighted clustering")
#cluster_sizes_sqrt = [math.sqrt(s) for _,s in cluster_sizes]
#km_weighted = km.transpose().multiply(cluster_sizes_sqrt).transpose()
#km_weighted.to_csv(f"{args.out_prefix}.km_weighted.tsv", sep="\t")
