This is a continuation of the previous puzzle, the same rules apply regarding the mouse's
movements.


THere is now a trap - big red and circular between the mouse and it's dinner. The mouse
begins its journey at position (0,0); the trap aways at (5,2); the delicious cheese is
located at (7, 4).

If the mouse chooses a mouse-to-cheese path at uniformly at random, what is the
probability that it survives the trip?

```
 01234567
4┌┬┬┬┬┬┬C
3├┼┼┼┼┼┼┤
2├┼┼┼┼T┼┤
1├┼┼┼┼┼┼┤
0M┴┴┴┴┴┴┘
```

We start with the same number of moves as before 11!/(7!4!) = 330.

There are 7!/(5!2!) ways to get to the trap and 4!/2!/2! ways to get from the trap to the
end to complete those failed paths.

So `(7!/(5!2!)*4!/(2!2!)) / 330 = (7*6*4*3/4) / 330 = 126/330 = 21/55` failed paths and
34/55 successful paths.
