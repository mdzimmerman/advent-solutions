#!/usr/bin/python

file="input.txt"

n=1
floor=0
fh=open(file, 'r')
for line in fh:
    for c in list(line):
        if c == "(":
            floor += 1
        elif c == ")":
            floor -= 1
        if floor == -1:
            print "%d %s %3d" % (n, c, floor)
        n += 1

print "final = %d" % floor

