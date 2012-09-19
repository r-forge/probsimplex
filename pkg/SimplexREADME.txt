Contains three functions.

Simfunc(a,b) returns the Simplex co-ordinates of probability distribution (a,b,1-a-b), assuming (1,0,0) has co-ordinates (0,0), and the vertical axis represents changes in the second component.

Sim(x,y,a,b) is a function which plots probabilities in the Simplex using Simfunc. x is a vector of first component probabilities. y is a vector of second component probabilities. a is the colour of the points plotted. b is the pch of the points plotted.

Example:

Sim(c(0,0.4,0.7,0.6,0.3),c(0.6,0.2,0.1,0.4,0.6),2,19)



Simline(x,y,a) is as above, but connects the points with a line, in the order they are typed, hence removing the need for a pch value. x,y and a are as above.

Example:

Simline(c(0,0.4,0.7,0.6,0.3,0),c(0.6,0.2,0.1,0.4,0.6,0.6),2)