# About this app

[`cutpointr`](https://github.com/Thie1e/cutpointr) is an R package to determine and evaluate optimal cutpoints in binary classification tasks. This app is a user friendly interface to that package. It supports several methods for calculating cutpoints and includes several metrics that can be maximized or minimized. Some of these methods are designed to be more robust than the simple empirical optimization of a metric. Additionally, [`cutpointr`](https://github.com/Thie1e/cutpointr) can bootstrap the variability of the optimal cutpoints and the corresponding metrics and return out-of-bag estimates of various performance metrics.

The Simulation tab can be used to run simple simulation studies for assessing the variability of an optimal cutpoint depending on the effect size (the distributions of positive and negative cases), the metric and the cutpoint estimation method.

More details on the package and the optimization methods are given in Thiele, C., & Hirschfeld, G. (2021). cutpointr: Improved Estimation and Validation of Optimal Cutpoints in R. Journal of Statistical Software, 98(11), 1–27. https://doi.org/10.18637/jss.v098.i11

Readme and code can be found on Github: https://github.com/Thie1e/cutpointr

Contact the author: christian.thiele (at) fh-bielefeld.de

Supported by BMBF Indimed (grant 01EK1501).
