### By Myra; testing input creation for Lingustic model.

import numpy
from random import randint, shuffle, random
import sys

### :-) These params would end up in the setting class in scads, so
### they're just global here.

n_inputs = 5
noise_scale = 0.05

# :-) Made this a class which will make it much simler to move the
# whole thing into your new model.

class lexical_inputs(object): 

	# :-) The dictionary is a local to the lexical_inputs object.
      input_dictionary = {}

      # :-) Init will fill the dictionary with random numbers. We
      # don't ever want to change these. Instead, copy them in the
      # noisifying process.
      def __init__(self):
	      for i in range(1,n_inputs+1):
		self.input_dictionary[i] = [randint(0, 1) for x in range(n_inputs)] # :-) Used a fancy comprehension here

      # :-) This just noisifies a single value at a time. I'll get called
      # over and over in a map over the list of values.
      def noisify(self,v):
	      noise = numpy.random.normal(loc=0.0, scale=noise_scale)
			#scale is SD
			#absolute value of noise? since no negative values
	      if v == 0:
		      return (v + abs(noise))
	      else:
		      return (v - abs(noise))

      # :-) This is the main function that a user will call. It just
      # creates a COPY of the representation, with noise.
      def addendWithNoise(self,a): 
	      r = self.input_dictionary[a]
	      return [self.noisify(r[x]) for x in range(n_inputs)] # :-) Comprehension again!
      
		      
### Just for display purposes, don't need all those decimal places.
def Rstr(l):
	return str(['{0:.5f}'.format(v) for v in l])

### Testing

indict = lexical_inputs() # :-) Init the dictionary

print str(5) + ":" + Rstr(indict.input_dictionary[5])

print str(5) + ":" + Rstr(indict.addendWithNoise(5))

print("Dictionary:")

for k, v in indict.input_dictionary.items():
        print(str(k) + " : " + Rstr(v))

print("Expample addition inputs:")

for i in range(10):
	a1 = randint(1,5)
	a2 = randint(1,5)
	print(str(a1) + ":" + Rstr(indict.addendWithNoise(a1)) + "+" + str(a2) + ":" + Rstr(indict.addendWithNoise(a2)))
