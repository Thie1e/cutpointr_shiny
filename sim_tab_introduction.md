You can define distributions for both classes here and simulate the cutpoint estimation.

The result will be a distribution of optimal cutpoints based on the specified distributions, sample sizes, and cutpoint estimation method.

This distribution can be used to estimate how precise the specified cutpoint estimation will be. This might be useful in the planning phase of a study, for example, to determine the necessary sample size for a certain precision.

### Simulation procedure

Repeat 'simulation runs' times:

1. Generate data with specified distributions and sample size
2. Calculate optimal cutpoint with specified method/metric

Use the percentiles of the distribution of the optimal cutpoints as an estimate of the expected precision of the cutpoint estimation.

### Instructions

1. Define distributions for the positive and negative classes separately. The chosen distributions are plotted in the 'Distributions' tab. 
2. To start the simulation, switch to the 'Cutpoint Distribution' tab. Define how many repetitions of the simulation are run and select a method for cutpoint optimization. Click 'Start'. 

For sample size planning, first set the distributions to realistic values and then tweak the group sizes until a desired width of the confidence interval is reached.

### Example

In the planning phase of a study, we expect the data that will be obtained to
have two classes that are each normally distributed, each with
a size of n = 100, with means of 50 and 60, and with standard deviations of 10 and 15. 
This data should be entered in the "Distributions" tab.

We want to estimate an optimal cutpoint for the Youden-index using bootstrapping 
(maximize_boot_metric in cutpointr) with 100 bootstrap repetitions for the
cutpoint estimation. This should be entered in the "Cutpoint
Distribution" tab.

The sample size of the study shall be planned such that the estimated cutpoint
falls within an interval with a maximum width of 10 with 95% probability based
on the simulation percentiles. What is the required sample size?

We run the simulation 500 times with a seed of 1234. This should also be 
entered in the "Cutpoint Distribution" tab.

The result is a distribution of optimal cutpoints with 2.5% and 97.5% 
percentiles of 53.71 and 62.71, respectively. The width of the confidence
interval around the optimal cutpoint is thus 9, given the above cutpoint 
estimation method, distributions, and sample sizes. 

Accordingly, the sample size of n = 200 is expected to suffice for the desired
precision.
