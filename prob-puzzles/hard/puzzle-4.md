You enter a metro station in a big hurry, and decide to take the first train that arrives.

There are two lines running thrtough this station: one runs every five minutes (line A),
the other every three (line B). to be precise, suppose the next arrival of the A train is
uniformly distributed on the interval [0, 5], and similaly for the B train on [0,3]. The
two arrivals are independent.

The trains run like clockwork, there's no uncertainty other than the next arrival
time. For example, given that the next B train arrives at time 0.87, you can be absolutely
certain that there will be another at time 3.87.

How many minutes on average will you wait on average until you get a train.

So the simple way is to define a delta and loop over the possible values of the function
over the 2 variables. Then divide by the count and the delta. Once you have that as a
basis, you can compact the inner loop by breaking it into two smaller loops, one where i
<= j and the other for i > j. Then you can use summation formulas to compact those into
simple multiplications.

From there, it is pretty simple to convert it into the equivalent definite integral (it
will be essentially the same expression). Thus giving you the exact answer.

```
E(X) = Min(E(3), E(5))
     = S(x=0,3: 1/2x*x + x*(5-x))/15
     = S(x=0,3: 1/2x^2 + 5x - x^2)/15
     = 18/15 = 1.2
```
