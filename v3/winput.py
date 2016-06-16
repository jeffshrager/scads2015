import numpy
from random import randint, shuffle, random
import sys

input = [0] * 5
def probpres():
	for i in range(0,5):
		input[i] = [randint(0, 1), randint(0, 1), randint(0, 1), randint(0, 1), randint(0, 1)]
	return input

def injectNoise():
	for i in range(0,5):
		for j in range(0,5):
			noise = numpy.random.normal(loc=0.0, scale=0.05)
			#scale is SD
			#absolute value of noise? since no negative values
			if input[i][j] == 0:
				input[i][j] += abs(noise)
			else:
				input[i][j] -= abs(noise)
	return input

print probpres()
print injectNoise()