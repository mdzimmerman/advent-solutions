import re
import numpy as np

linere = re.compile("(\w+) to (\w+) = (\d+)")

city = {}
n = 0
c = np.zeros((8,8), dtype=np.int)

f = "input.txt"
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

print city
print c
