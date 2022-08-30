Select a method and a metric for calculating an optimal cutpoint for the data that was uploaded on the tab 'Data Upload'.

Methods:

- maximize_metric: Maximize /minimize the selected metric.
- maximize_boot_metric / minimize_boot_metric: Maximize / minimize the selected metric as the mean optimal cutpoint from a number of bootstrap samples.
- oc_youden_kernel: Calculate the cutpoint that maximizes the Youden Index after applying kernel density smoothing to the distribution of the positive and negative classes. No metric plot will be drawn for this method.
- oc_youden_normal: Calculate the optimal cutpoint parametrically by assuming normal distributions of the positive and negative classes.

More details on the package and the optimization methods are given in Thiele, C., & Hirschfeld, G. (2021). cutpointr: Improved Estimation and Validation of Optimal Cutpoints in R. Journal of Statistical Software, 98(11), 1–27. https://doi.org/10.18637/jss.v098.i11