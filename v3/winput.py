### By Myra; testing input creation for Lingustic model.

import numpy
from random import randint, shuffle, random
import sys
import copy # :-) Needed to copy the list.

### :-) These params would end up in the setting class in scads, so
### they're just global here.

n_inputs = 5
noise_scale = 0.05

class lexical_inputs(object): # :-) Made this a class

      input_dictionary = {}

      def __init__(self):
	      for i in range(1,n_inputs+1):
		self.input_dictionary[i] = [randint(0, 1) for x in range(n_inputs)] # :-) Used a fancy comprehension here

      def noisify(self,v):
	      noise = numpy.random.normal(loc=0.0, scale=noise_scale)
			#scale is SD
			#absolute value of noise? since no negative values
	      if v == 0:
		      return (v + abs(noise))
	      else:
		      return (v - abs(noise))

      def addendWithNoise(self,a): # :-) Rewrote to just get one COPY of the target number with noise added. 
	      r = copy.copy(self.input_dictionary[a])
	      return [self.noisify(r[x]) for x in range(n_inputs)] # :-) Comprehension again!
      
		      
def Rstr(l):
	return str(['{0:.5f}'.format(v) for v in l])

### Testing

indict = lexical_inputs() # :-) Init the dictionary

for i in range(10):
	a1 = randint(1,5)
	a2 = randint(1,5)
	print(str(a1) + ":" + Rstr(indict.addendWithNoise(a1)) + "+" + str(a2) + ":" + Rstr(indict.addendWithNoise(a2)))


