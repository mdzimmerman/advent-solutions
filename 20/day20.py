# -*- coding: utf-8 -*-

import numpy as np

N=36000000
#N=100

a=np.zeros(N//10)

for i in range(1, N//10+1):
    a[i::i] += i * 10

print(np.argwhere(a > N)[0])

#
#h=0
#c=0
#while c < n:
#    h+=1
#    if h % 10000 == 0:
#        print(h)
#    c=0
#    for e in range(1, h+1):
#        if h % e == 0:
#            c+=10*e
#    
#print("%7d %10d" % (h, c))

#print(a)