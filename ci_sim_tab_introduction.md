The result of this simulation are bootstrapped confidence intervals, based on the percentiles of the bootstrap distribution of optimal cutpoints. By defining the true optimal cutpoint, the coverage probability of bootstrapped confidence intervals can be simulated for specified distributions, sample sizes, cutpoint estimation methods, and metrics.

The true cutpoint either has to be calculated analytically (feasible for normal distributions) or arrived at via simulation (previous tab).

### Simulation procedure

Repeat 'simulation runs' times:

1. Generate data with specified distributions and sample size
2. Draw bootstrap sample from the generated data set from step 1
3. Calculate optimal cutpoints with specified method/metric for the bootstrap samples from step 2
4. Use this distribution of optimal cutpoints to form a confidence interval, using the 2.5% and 97.5% percentiles

This results in a number of 'repetition' bootstrapped confidence intervals. The percentage of confidence intervals that contains the specified true optimal cutpoint is the estimated coverage probability.

### Instructions

1. Define distributions for the positive and negative classes separately. The chosen distributions are plotted in the 'Distributions' tab. 
2. To start the simulation, switch to the 'CI coverage simulation' tab. Define how many repetitions of the simulation are run and select a method for cutpoint optimization. For the coverage probability, the true optimal cutpoint has to be known and entered as well. Confidence intervals are calculated using percentiles from a nonparametric bootstrap. The number of repetitions of this bootstrap can be defined under "Confidence Interval for Cutpoint". Click 'Start'. 

### Example

We have data with two classes that are each normally distributed, each with
a size of n = 100, with means of 50 and 60, and both with a standard deviation of 10. 
This data should be entered in the "Distributions" tab.

We want to know if bootstrap confidence intervals using 1000 bootstrap repetitions
for the F1 score and simple empirical maximization (maximize_metric) 
have an appropriate coverage probability. This should be entered in the 
"CI coverage simulation" tab. The true optimal cutpoint is approximately 49.6.

We run the simulation 500 times with a seed of 1234. Since we do 1000 bootstrap repetitions 500
times, this simulation is computationally demanding and takes about 15 minutes.

The result is a distribution of bootstrapped confidence intervals around the
optimal cutpoints for every simulation run (the first 100 are plotted).
Thus, for the defined distributions and empirical maximization of the F1-score,
the coverage of bootstrapped confidence intervals is around 96.8%. The intervals
have a mean width of around 30.
