#!/usr/bin/python

input="input2.txt"

fh=open(input,"r")
total=0
for line in fh:
    line=line.rstrip()
    #print line
    (x, y, z) = [int(i) for i in line.split("x")]
    area = sorted([x*y, y*z, x*z])
    totalarea = 2*area[0] + 2*area[1] + 2*area[2] + minarea
    total += totalarea
    print "dim: %2d %2d %2d areas: %4d %4d %4d minarea: %4d totalarea: %5d TOTAL: %7d" % (x, y, z, area[0], area[1], area[2], minarea, totalarea, total)
