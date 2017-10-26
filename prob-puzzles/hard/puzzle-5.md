Five men and eight women will be seated at random around a circular table, in such a way
thatr all orderings are equally likely.

Let's assume everyone is a heterosexual (and single), so that there is a potential couple
any time a man and a woman are sitting next to each other. Note that a person can be part
of (up to) two potential couples: one with the person to their left and one with the
person to their right.

On average, how many potential couples will be seated at the table?

Solve the same way as the fox/hound problem.

```
E(woman) = 2*(5/12)(4/11) + 1*(5/12)(7/11) + 1*(7/12)(5/11) + 0*(7/12)(6/11)
E(couples) = 8*E(woman) = ~ 6 + 2/3
```
