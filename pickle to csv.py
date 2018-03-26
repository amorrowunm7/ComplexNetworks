import pickle
n = open("nodes.follow1.set", "rb")
nodes = pickle.load(n)

e = open("edges.follow2.dict", "rb")
edges = pickle.load(e)

f = open("following1", "rb")
following = pickle.load(f)

# Hide some silly output
import logging
logging.getLogger("requests").setLevel(logging.WARNING)
logging.getLogger("urllib3").setLevel(logging.WARNING)

# Import everything we need
#

# Generate CSVs from the previous crawl
# TODO
#f = open('vertices_heritage.csv', 'w')
#f.write('id\n')
#for node in nodes:
 #   f.write(str(node) + "\n")
#f.close()

f = open('edges.csv', 'w')
f.write('src,dst,relation\n')
for node, followers in edges.items():
    for follower in followers:
        f.write('%s,%s,%s\n' % (follower, node, 'follows'))
f.close()




import csv
from six.moves import cPickle as pickle
import numpy as np



def main(e,path_csv):

    x = []
    with open(e,'rb') as f:
        x = pickle.load(f)

    with open(path.csv,'wb') as f:
        writer = csv.writer(f)
        for line in x: writer.writerow(line)



