You are given N distinct objects from which you draw a random sample of size N with
replacement -- that means objects are allowed to be sampled multiple times. Generating a
sample in this way is related to the statistical technique of bootstrapping.

Were' interested in the expected fraction of the original collection that ends up in the
sample (i.e. the probability that a given object int he original collection appears at
least once in the sample). What does this probability converge to as N goes to infinity.

The probability of item 1 never getting sampled in any given selection is (n-1)/n, or equivalently
1-1/n.  We're looking for the probability of doing that n times. So (1-1/n)^n. That's 1/e.

Thus our final probability is 1 - 1/e = 63.21%
