import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random
from types import *


input_one_bits = 5
input_dictionary = {}

#new
fmt = "{0:0"+str(10)+"b}"
r = []
for i in range(1025):
    s = fmt.format(i)
    if s.count('1') == 5:
        r.extend([s])

for k in range(len(r)):
  r[k]=[int(c) for c in r[k]]

shuffle(r)
print r

for k in range(1,11):
    input_dictionary[k]=[int(c) for c in r[k-1]]
print input_dictionary



print r