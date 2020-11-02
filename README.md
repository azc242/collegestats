# collegestats
A package originally designed for ECON-18 statistics course at NYU, but contains useful R functions for most university statistics classes.

# How to Install
If you don't have `devtools` installed, you can do so by running the following code in your terminal or RStudio
If you have ``devtools`` installed, just run the second line to install `collegestats`.
```
install.packages("devtools")
devtools::install_github("azc242/collegestats")
```
# Functions
## get_p_value(z_score, two_sided = FALSE)
**Arguments** 
- z_score: z-score
- two_sided: Boolean value denoting whether test is double sided or not, defaults to FALSE

**Value**
P-value

## get_z(p_hat, p_null, size, sigma = NULL)
**Arguments** 
- p_hat: Sample probability
- p_null: Null hypothesis value
- size: Sample size
- sigma: (Optional) standard deviation, defaults to NULL

**Value**
Z-score

## get_z_given_sd(p_hat, p_null, sd, size)
**Note: this function should NOT be used, use ``get_z()`` instead and pass the standard deviation as ``sigma``.**
**Arguments** 
- p_hat: Sample probability
- p-null: Null hypothesis value
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

## type_2_err(Ho, Ha, p, n, alpha, sigma = NULL)
**Arguments** 
- Ho: Null hypothesis
- Ha: Direction of the test, valid arguments are either "<", ">", or "!="
- p: True probability
- n: Sample size
- alpha: Significance level/Type I error probability/alpha level
- sigma (Optional) population standard deviation

**Value**
Minimum sample size needed to obtain desired Beta, or Type II error.
## type_2_err_min_size(alpha, beta, Ho, Ha, two_sided = FALSE, sigma = NULL)
**Arguments** 
- alpha: Probability of Type I error
- beta: Probability of Type II error desired
- Ho: Null hypothesis
- Ha: Actual mean or probability used with beta
- two_sided: (Optional) Boolean value denoting whether test is double sided or not, defaults to FALSE
- sigma: (Optional) Standard deviation. Defaults to NULL, for when Ho and Ha are probabilities and not mean values

**Value**
NOT rounded minimum sample size needed (generally round upwards for an upper test)
