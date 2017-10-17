A hungry mouse is sitting at the SW corner of a 4 by 7 tile surface, beautifully
illustrated below. The mouse sniffs around and detects a delectable piece of cheese at the
NE corner.

Suppose the mouse moves only along the edges of tiles, and chooses to travel only
northward or eastward, as it would like to reach its meal rapidly. For example ,thge mouse
might reach the cheese by moving north three times, east seven times, then north once.

How many different paths can the mouse take to reach the cheese?

```
 01234567
4┌┬┬┬┬┬┬C
3├┼┼┼┼┼┼┤
2├┼┼┼┼┼┼┤
1├┼┼┼┼┼┼┤
0M┴┴┴┴┴┴┘
```

We have 4 vertical and 7 horizontal moves in every path. So 11!/7!4! possible moves (330).
