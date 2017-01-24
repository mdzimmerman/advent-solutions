import re
import numpy as np
import itertools

def distance(permute, cost):
    n0 = permute[0]
    dist = 0
    for n in permute[1:]:
        dist += cost[n0,n]
        n0 = n
    return dist

linere = re.compile("(\w+) to (\w+) = (\d+)")

city = {}
n = 0
c = np.zeros((8,8), dtype=np.int)

f = "input2.txt"
with open(f, "r") as fh:
    for l in fh:
        l = l.rstrip()
        m = linere.match(l)
        if m:
            c1, c2, cost = m.groups()
            c1n = -1
            c2n = -1
            if c1 not in city:
                city[c1] = n
                n += 1
            if c2 not in city:
                city[c2] = n
                n += 1
            c[city[c1],city[c2]] = int(cost)
            c[city[c2],city[c1]] = int(cost)

            #print c2
            #print cost

minp=None
mindist=distance(range(0,8), c)
maxp=None
maxdist=0

i = 0
for p in itertools.permutations(range(0,8),8):
    dist = distance(p, c)
    if dist < mindist:
        mindist = dist
        minp = p
    elif dist > maxdist:
        maxdist = dist
        maxp = p
    print i, p, dist
    i += 1

print "min:", minp, mindist
print "max:", maxp, maxdist
