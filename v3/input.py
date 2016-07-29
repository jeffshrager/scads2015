import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random
from types import *


#testing outputs now
output_one_bits = 3 # !!! This should be 3 here, and then replicate this again forthe 5:2 part.
output_dictionary={}

#5:3 part
fmt2 = "{0:0"+str(5)+"b}"
v = [x for x in range(2**5)]
r = []
while len(r) < 10:
    n = randint(0,len(v)-1)
    s = fmt2.format(v[n])
    if s.count('1') == output_one_bits:
        r.extend([s])
print r

#5:2 part
w = [x for x in range(2**5)]
o = []
while len(o) < 10:
    n = randint(0,len(w)-1)
    s = fmt2.format(w[n])
    print type(s)
    if s.count('1') == 2:
        o.extend([s])
print o
for k in range(1,11):
    output_dictionary[k]=[int(c) for c in r[k-1]] + [int(c) for c in o[k-1]]

print output_dictionary