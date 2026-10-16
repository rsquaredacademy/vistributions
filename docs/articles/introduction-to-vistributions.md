# Exploring Distributions

In exploring statistical distributions, we focus on the following:

- what influences the shape of a distribution
- calculate probability from a given quantile
- calculate quantiles out of given probability

To explore the above 3 concepts, we have defined functions for the
following distributions:

- Normal
- Binomial
- Chi Square
- F
- t

## Normal Distribution

### Distribution Shape

Visualize how changes in mean and standard deviation affect the shape of
the normal distribution.

##### Input

- mean: mean of the normal distribution
- sd: standard deviation of the normal distribution

##### Output

- Normal distribution plot

[`vdist_normal_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``mean ``=`` ``2``, sd ``=`` ``0.6``)`

![](introduction-to-vistributions_files/figure-html/norm_plot-1.png)

### Percentiles

#### Calculate and visualize quantiles out of given probability.

##### Input

- probs: a probability value
- mean: mean of the normal distribution
- sd: standard deviation of the normal distribution
- type: lower/upper tail

Suppose X, the grade on a exam, is normally distributed with mean 60 and
standard deviation 3. The teacher wants to give 10% of the class an A.
What should be the cutoff to determine who gets an A?

[`vdist_normal_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``0.10``, ``60``, ``3``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/norm_per1-1.png)

The teacher wants to give lower 15% of the class a D. What cutoff should
the teacher use to determine who gets a D?

[`vdist_normal_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``0.15``, ``60``, ``3``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/norm_per2-1.png)

The teacher wants to give middle 50% of the class a B. What cutoff
should the teacher use to determine who gets an B?

[`vdist_normal_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``0.5``, ``60``, ``3``, ``'both'``)`

![](introduction-to-vistributions_files/figure-html/norm_per3-1.png)

### Probabilities

#### Calculate and visualize probability from a given quantile

##### Input

- perc: a quantile value
- mean: mean of the normal distribution
- sd: standard deviation of the normal distribution
- type: lower/upper/both tail

Let X be the IQ of a randomly selected student of a school. Assume X ~
N(90, 4). What is the probability that a randomly selected student has
an IQ below 80?

[`vdist_normal_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``80``, mean ``=`` ``90``, sd ``=`` ``4``)`

![](introduction-to-vistributions_files/figure-html/norm_prob1-1.png)

What is the probability that a randomly selected student has an IQ above
100?

[`vdist_normal_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(``100``, mean ``=`` ``90``, sd ``=`` ``4``, type ``=`` ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/norm_prob2-1.png)

What is the probability that a randomly selected student has an IQ
between 85 and 100?

[`vdist_normal_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_normal_plot.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``85``, ``100``)``, mean ``=`` ``90``, sd ``=`` ``4``, type ``=`` ``'both'``)`

![](introduction-to-vistributions_files/figure-html/norm_prob3-1.png)

## Binomial Distribution

### Distribution Shape

Visualize how changes in number of trials and the probability of success
affect the shape of the binomial distribution.

[`vdist_binom_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``10``, ``0.3``)`

![](introduction-to-vistributions_files/figure-html/binom_plot-1.png)

### Percentiles

#### Calculate and visualize quantiles out of given probability

##### Input

- p: a single aggregated probability of multiple trials
- n: the number of trials
- tp: the probability of success in a trial
- type: lower/upper tail

[`vdist_binom_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``10``, ``0.5``, ``0.05``)`

![](introduction-to-vistributions_files/figure-html/binom_per1-1.png)

[`vdist_binom_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``10``, ``0.5``, ``0.05``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/binom_per2-1.png)

### Probabilities

#### Calculate and visualize probability from a given quantile

##### Input

- p: probability of success
- n: the number of trials
- s: number of success in a trial
- type: lower/upper/interval/exact tail

Assume twenty-percent (20%) of Magemill have no health insurance.
Randomly sample n = 12 Magemillians. Let X denote the number in the
sample with no health insurance. What is the probability that exactly 4
of the 12 sampled have no health insurance?

[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, ``4``, type ``=`` ``'exact'``)`

![](introduction-to-vistributions_files/figure-html/binom_prob1-1.png)

What is the probability that at most one of those sampled has no health
insurance?

[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, ``1``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/binom_prob2-1.png)

What is the probability that more than seven have no health insurance?

[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, ``8``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/binom_prob3-1.png)

What is the probability that fewer than 5 have no health insurance?

[`vdist_binom_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_binom_plot.md)`(``12``, ``0.2``, `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``4``)``, ``'interval'``)`

![](introduction-to-vistributions_files/figure-html/binom_prob4-1.png)

## Chi Square Distribution

### Distribution Shape

Visualize how changes in degrees of freedom affect the shape of the chi
square distribution.

[`vdist_chisquare_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``df ``=`` ``5``)`

![](introduction-to-vistributions_files/figure-html/chi_plot-1.png)

[`vdist_chisquare_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``df ``=`` ``5``, normal ``=`` ``TRUE``)`

![](introduction-to-vistributions_files/figure-html/chi_plot-2.png)

### Percentiles

#### Calculate quantiles out of given probability

##### Input

- probs: a probability value
- df: degrees of freedom
- type: lower/upper tail

Let X be a chi-square random variable with 8 degrees of freedom. What is
the upper fifth percentile?

[`vdist_chisquare_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``0.05``, ``8``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/chi_per1-1.png)

What is the tenth percentile?

[`vdist_chisquare_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``0.10``, ``8``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/chi_per2-1.png)

### Probability

#### Calculate probability from a given quantile.

##### Input

- perc: a quantile value
- df: degrees of freedom
- type: lower/upper tail

What is the probability that a chi-square random variable with 12
degrees of freedom is greater than 8.79?

[`vdist_chisquare_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``8.79``, ``12``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/chi_prob1-1.png)

What is the probability that a chi-square random variable with 12
degrees of freedom is less than 8.62?

[`vdist_chisquare_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_chisquare_plot.md)`(``8.62``, ``12``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/chi_prob2-1.png)

## F Distribution

### Distribution Shape

Visualize how changes in degrees of freedom affect the shape of the F
distribution.

[`vdist_f_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``)`

![](introduction-to-vistributions_files/figure-html/f_plot-1.png)

[`vdist_f_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``6``, ``10``, normal ``=`` ``TRUE``)`

![](introduction-to-vistributions_files/figure-html/f_plot-2.png)

### Percentiles

#### Calculate quantiles out of given probability

##### Input

- probs: a probability value
- num_df: numerator degrees of freedom
- den_df: denominator degrees of freedom
- type: lower/upper tail

Let X be an F random variable with 4 numerator degrees of freedom and 5
denominator degrees of freedom. What is the upper twentieth percentile?

[`vdist_f_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``0.20``, ``4``, ``5``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/f_per1-1.png)

What is the 35th percentile?

[`vdist_f_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``0.35``, ``4``, ``5``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/f_per2-1.png)

### Probabilities

#### Calculate probability from a given quantile.

##### Input

- perc: a quantile value
- num_df: numerator degrees of freedom
- den_df: denominator degrees of freedom
- type: lower/upper tail

What is the probability that an F random variable with 4 numerator
degrees of freedom and 5 denominator degrees of freedom is greater than
3.89?

[`vdist_f_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``3.89``, ``4``, ``5``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/f_prob1-1.png)

What is the probability that an F random variable with 4 numerator
degrees of freedom and 5 denominator degrees of freedom is less than
2.63?

[`vdist_f_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_f_plot.md)`(``2.63``, ``4``, ``5``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/f_prob2-1.png)

## t Distribution

### Distribution Shape

Visualize how degrees of freedom affect the shape of t distribution.

[`vdist_t_plot`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``df ``=`` ``8``)`

![](introduction-to-vistributions_files/figure-html/t_plot-1.png)

### Percentiles

#### Calculate quantiles out of given probability

##### Input

- probs: a probability value
- df: degrees of freedom
- type: lower/upper/both tail

What is the upper fifteenth percentile?

[`vdist_t_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``0.15``, ``8``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/t_per1-1.png)

What is the eleventh percentile?

[`vdist_t_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``0.11``, ``8``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/t_per2-1.png)

What is the area of the curve that has 95% of the t values?

[`vdist_t_perc`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``0.95``, ``8``, ``'both'``)`

![](introduction-to-vistributions_files/figure-html/t_per3-1.png)

### Probabilities

#### Calculate probability from a given quantile.

##### Input

- perc: a quantile value
- df: degrees of freedom
- type: lower/upper/interval/both tail

Let T follow a t-distribution with r = 6 df.

What is the probability that the value of T is less than 2?

[`vdist_t_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``2``, ``6``, ``'lower'``)`

![](introduction-to-vistributions_files/figure-html/t_prob1-1.png)

What is the probability that the value of T is greater than 2?

[`vdist_t_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``2``, ``6``, ``'upper'``)`

![](introduction-to-vistributions_files/figure-html/t_prob2-1.png)

What is the probability that the value of T is between -2 and 2?

[`vdist_t_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``2``, ``6``, ``'interval'``)`

![](introduction-to-vistributions_files/figure-html/t_prob3-1.png)

What is the probability that the absolute value of T is greater than 2?

[`vdist_t_prob`](https://vistributions.rsquaredacademy.com/reference/vdist_t_plot.md)`(``2``, ``6``, ``'both'``)`

![](introduction-to-vistributions_files/figure-html/t_prob4-1.png)
