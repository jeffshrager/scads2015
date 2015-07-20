import ADD
import NeuralNetwork as nn1
import numpy as np
import random

def main():
    #this is the addends matrix
    input_units = 14
    #this will be looped over, the np.arrange thing
    hidden_units = 30
    #this is the output matrix
    output_units = 13


    #fits to counting network
    nn = nn1.NeuralNetwork([input_units,hidden_units,output_units])
    X_count = []
    y_count = []
    for i in range(1,5):
        X_count.append(nn1.addends_matrix(i,i+1))
        y_count.append(nn1.sum_matrix(i+2))
    X_count = np.array(X_count)
    y_count = np.array(y_count)
    nn.fit(X_count,y_count,0.07)





main()