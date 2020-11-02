# collegestats
A package originally designed for ECON-18 statistics course at NYU, but contains useful R functions for most university statistics classes.

# How to Install
If you don't have `devtools` installed, you can do so by running the following code in your terminal or RStudio
```install.packages("devtools")```
Then, run the following line to install `collegestats`.
```
devtools::install_github("azc242/collegestats")
```
# Functions
## getP(zScore, doubleSided = FALSE)
**Arguments** 
- zScore: z-score
- doubleSided: Boolean value denoting whether test is double sided or not, defaults to FALSE

**Value**
P-value

## getZ(pHat, pNull, size)
**Arguments** 
- pHat: Sample probability
- pNull: Null hypothesis value
- size: Sample size

**Value**
Z-score

## getZGivenSD(pHat, pNull, sd, size)
**Arguments** 
- pHat: Sample probability
- pNull: Null hypothesis value
- sd: Standard deviation
- size: Sample size

**Value**
Z-score

## interval(trials, successes, conf)
**Arguments** 
- trials: Number of trials
- successes: Number of successes of the trials
- conf: Confidence level

**Value**
Confidence level formatted as a String

## type2err(Ho, Ha, p, n, alpha)
**Arguments** 
- Ho: Null hypothesis
- Ha: Direction of the test, valid arguments are either "<", ">", or "!="
- p: True probability
- n: Sample size
- alpha: Significance level/Type I error probability/alpha level

**Value**
Probability of Type II error