Suppose that in the general population, mathematical and writing ability are independently
and uniformly distributed on [0,1].

People attend university if and only iff the sum of their writing and math abilities is
larger than one. Among people who've attended univeristy, what fraction have math ability
above 0.9, i.e. in the top 10% of the general population?

So first observation is that `P(Attending University|Math=x) = x`. This is because the sum
of math and reading is 1, so reading must be at least `1-x`, which occurs with probability
`1 - (1-x) = x`

I took the integral of that from 0.9 to 1.0 which gives you the probability of attending
university if math is at least 0.9, which we then need to rebase to just the university
population. Luckily half the population in this world attend university, so we just need
to double that value.
