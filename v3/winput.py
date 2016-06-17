### By Myra; testing input creation for Lingustic model.

import numpy
import decimal
from random import randint, shuffle, random
import sys
import copy

### These params would end up in the setting class in scads, so 
### they're just global here.

n_inputs = 5
noise_scale = 0.05

class lexical_inputs(object): # :-) Made this a class

      input_dictionary = [0] * n_inputs

      def __init__(self):
	      for i in range(0, n_inputs):
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
	      r = copy.copy(self.input_dictionary[1])
	      return [self.noisify(r[x]) for x in range(n_inputs)] # :-) Comprehension again!
      
		      
def Rstr(l):
	return str(['{0:.5f}'.format(v) for v in l])

indict = lexical_inputs() # :-) Init the dictionary array (It's actually not a dictionary!)
### Testing
for i in range(10):
	a1 = randint(1,5)
	a2 = randint(1,5)
	print(str(a1) + ":" + Rstr(indict.addendWithNoise(a1)) + "+" + str(a2) + ":" + Rstr(indict.addendWithNoise(a2)))

#print init_input_dictionay()
#print injectNoise()
