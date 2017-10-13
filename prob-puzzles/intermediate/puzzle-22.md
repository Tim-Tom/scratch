You have an equal number of red and black balls in a urn. You pull out two of the same
color. The probability of pulling out two of the same color (not neccessarily same as the
first time) is now 0.5, how many balls were in the urn intially?

This is just a algebra problem. You could easily iterate through the first 1000 sizes and
find it immediately, but it's equally easy to do the algebra.

Let's assume we picked out two red balls the first time (since it's arbitrary and already happened)

So: `R + 2 = B` 

```
:Given:
R + B = T
R + 2 = B
(R/T)*((R-1)/(T-1)) + (B/T)*((B-1)/(T-1)) = 1/2

:Simplify T:
R + R + 2 = T : B = R + 2
2R + 2 = T

:Simplify the last:
R(R-1) + B(B-1) = (1/2)T(T-1) : Cross Multiply
R(R-1) + (R+2)(R+1) = (1/2)(2R + 2)(2R + 1) : Substitute B = R + 2 and T = 2R + 2
R^2 - R + R^2 + 3R + 2 = (1/2)(4R^2 + 6R + 2) : Expand multiplication
2R^2 + 2R + 2 = 2R^2 + 3R + 1 : combine like terms and multiply on sides
R = 1 : Cancel terms

B = 1 + 2 = 3
```

So we started with 3 red and 3 black balls for a total of 6 balls.
