Run bootstrapping for the data itself and the cutpoint estimation procedure as defined in the previous two tabs. The result will be various bootstrapped statistics in tabular form and two plots:

1. Distribution of bootstrapped cutpoints: The distribution of the cutpoints that were calculated in the bootstrap samples
2. Out-of-bag distribution of the selected metric: As a way of internal validation, this plot shows the distribution of the metric values that were achieved by applying the cutpoints from the bootstrap samples to the portions of the data that were not part of the bootstrap samples (= the out-of-bag data)
