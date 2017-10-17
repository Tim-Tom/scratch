You are a trader considering a stock whose next price movement will be up or down with
equal probability. Fortunately, you have access to a binary signal that is predeictive of
the price change (and observable ahead of time). the signal is 60% accurate: conditional
on the stock going up, there's a 0.6 probability that the signal says up (and a 0.4
probability that it says down); the reverse is true if the stock goes down.

You have access to many such signals, all with the same accuracy. Assume they are
conditionally independent given thge price movement (e.g. conditional on the stock going
up, there's a 0.6^2 probabilitythat signals one and two both point up). Suppose you
observe N+1 signals pointing up and N pointing doqwn (out of a total of 2N+1
signals). What's the probability that the stock will go up?

Intuitively if you have two signals and they point up and down, they will cancel out and
your best answer is going to be the global probability of 50% on the signal. So we cancel
out a bunch of signals and are left with just one signal to consider. Giving us a
probability of 60% in that direction.

=cut

