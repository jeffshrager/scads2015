import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
from mpl_toolkits.mplot3d import Axes3D
import ADD

def sigmoid(x):
    return 1.0/(1.0 + np.exp(-x))

def sigmoid_prime(x):
    return sigmoid(x)*(1.0-sigmoid(x))

def tanh(x):
    return np.tanh(x)

def tanh_prime(x):
    return 1.0 - x**2

# Transform two addends into a binary input matrix.

def addends_matrix(a1,a2):
    lis = [0]*14
    lis[a1-1] = 1
    lis[a1] = 1
    lis[a1+1] = 1
    lis[a2+6] = 1
    lis[a2+7] = 1
    lis[a2+8] = 1
    return lis

# Transform the sum into a binary output matrix.

def sum_matrix(s):
    lis = [0]*13
    lis[s-1] = 1
    return lis

class NeuralNetwork:

    def __init__(self, layers, activation='tanh'):
        if activation == 'sigmoid':
            self.activation = sigmoid
            self.activation_prime = sigmoid_prime
        elif activation == 'tanh':
            self.activation = tanh
            self.activation_prime = tanh_prime

        # Set weights
        
        self.weights = []
        
        # range of weight values (-1,1)
        # input and hidden layers - random((2+1, 2+1)) : 3 x 3
        
        for i in range(1, len(layers) - 1):
            r = 2*np.random.random((layers[i-1] + 1, layers[i] + 1)) -1
            self.weights.append(r)
            
        # output layer - random((2+1, 1)) : 3 x 1
        
        r = 2*np.random.random( (layers[i] + 1, layers[i+1])) - 1
        self.weights.append(r)

    def fit(self, X, y, learning_rate=0.1, epochs=30000):
        
        # Add column of ones to X
        # This is to add the bias unit to the input layer
        
        ones = np.atleast_2d(np.ones(X.shape[0]))
        X = np.concatenate((ones.T, X), axis=1)
         
        for k in range(epochs):
            
            #if k % (epochs/10) == 0: print 'epochs:', k
            
            #choose a random training set
            
            i = np.random.randint(X.shape[0])
            a = [X[i]]

            for l in range(len(self.weights)):
                    dot_value = np.dot(a[l], self.weights[l])
                    activation = self.activation(dot_value)
                    a.append(activation)
                    
            # output layer
            
            error = y[i] - a[-1]
            deltas = [error * self.activation_prime(a[-1])]

            # we need to begin at the second to last layer 
            # (a layer before the output layer)
            
            for l in range(len(a) - 2, 0, -1): 
                deltas.append(deltas[-1].dot(self.weights[l].T)*self.activation_prime(a[l]))

            # reverse
            # [level3(output)->level2(hidden)]  => [level2(hidden)->level3(output)]
            
            deltas.reverse()

            # backpropagation
            # 1. Multiply its output delta and input activation 
            #    to get the gradient of the weight.
            # 2. Subtract a ratio (percentage) of the gradient from the weight.
            
            for i in range(len(self.weights)):
                layer = np.atleast_2d(a[i])
                delta = np.atleast_2d(deltas[i])
                self.weights[i] += learning_rate * layer.T.dot(delta)

    def predict(self, x): 
        a = np.concatenate((np.ones(1).T, np.array(x)), axis=1)      
        for l in range(0, len(self.weights)):
            a = self.activation(np.dot(a, self.weights[l]))
        return a

def train_and_test(hidden_units,learning_rate,epochs):
    
    # Set up a neural network with 10 units in the output layer
    # and 14 units in the input layer:
    # 7 units for one addend and 7 units for the other addend.
    # Here each addend corresponds to three units in the input layer,
    # and overlaps with numbers next to it.
    
    # For example,
    # If an addent is 1, the corresponding binary matrix is
    #      ==> [1, 1, 1, 0, 0, 0, 0]
    # If 2 ==> [0, 1, 1, 1, 0, 0, 0]
    # ...
    # If 5 ==> [0, 0, 0, 0, 1, 1, 1]
    # So 3 + 4 ==> [0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0]
    
    nn = NeuralNetwork([14,hidden_units,13])
    
    # Train the neural network.
    
    nn.fit(X, y, learning_rate, epochs)
    
    # Make predictions for each input set in X.
    
    prediction=np.array([nn.predict(X[0])])
    for e in range(1,X.shape[0]):
        prediction = np.append(prediction,[nn.predict(X[e])],axis=0)
    
    # Calculate the correlation coefficient between the correct 
    # output and the predicted output.
    
    return np.corrcoef(y.flatten(),prediction.flatten())[1,0]
    
def parameters(y_arg, epochs = 10000):
    global X,y

    # Set the range of parameters we want to test: number of 
    # units in the hidden layer, learning rate, and epochs. 
    
    hidden_units = np.arange(5,55,5)
    learning_rate = np.arange(0.01,0.11,0.01)

    # Setup input and output as binary matrices.
    
    X = []
    #y = []
    for i in range(5):
        for j in range(5):
            X.append(addends_matrix(i+1,j+1))
            #y.append(sum_matrix(i+j+2))
    X = np.array(X)
    y = np.array(y_arg)

    # Create axises for plotting purpose.
    
    axis1=[]
    axis2=[]
    axis3=[]
    
    # Iterate through different values of hidden_units and learning_rate
    # and get the correlation coefficient value from the function 
    # train_and_test.
    
    for i in hidden_units:
        a1=[]
        a2=[]
        a3=[]
        print "hidden units: %s" % (i)
        for j in learning_rate:
            a1.append(i)
            a2.append(j)
            lis = []
            print "learning rate coefficient: %s" % (j)
            
            # Run each parameter set 10 times and take the mean.
            
            for k in range(10):
                print "trials: %s" % (k)
                lis.append(train_and_test(i, j, epochs))
            a3.append(np.mean(lis))
            
        axis1.append(a1)
        axis2.append(a2)
        axis3.append(a3)
        
    paramters_graph(axis1,axis2,axis3)

def paramters_graph(axis1, axis2, axis3):
    # Plot hidden_units and learning_rates on x and y axises
    # and correlation coefficient on the z axis.
    fig = plt.figure()
    ax = fig.gca(projection='3d')

    surf = ax.plot_surface(axis1, axis2, axis3, rstride=1, cstride=1, cmap=cm.coolwarm,
        linewidth=0, antialiased=False)

    ax.zaxis.set_major_locator(LinearLocator(10))
    ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))

    fig.colorbar(surf, shrink=0.5, aspect=5)

    ax.set_xlabel('Number of Hidden Units')
    ax.set_ylabel('Learning Rate')
    ax.set_zlabel('Correlation Coefficient')
    plt.title('Epochs = 3000')
    plt.show()




