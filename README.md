# collegestats
A package originally designed for ECON-18 statistics course at NYU, but contains useful R functions for most university statistics classes.

# Installation
If you don't have `devtools` installed, you can do so by running the following code in your terminal or RStudio.
If you have ``devtools`` installed, just run the second line to install `collegestats`.
```
install.packages("devtools")
devtools::install_github("azc242/collegestats")
```
# Troubleshooting
**Trouble installing**: Refresh RStudio (ctrl/cmd + shift + F10), close RStudio, and retry.

**Show usage/docs**: `help(package = collegestats)`. This will only work if you have the package installed aready.

# Functions
## z_test_neq(H0, sigma = FALSE, alpha, samp_size, x_bar)
**Description**: Conducts double sided z-test, equivalent of Ha != H0.

**Arguments** 
- H0: Null hyothesis
- sigma = (Optional) population standard deviation
- samp_size: Sample size
- x_bar: Sample mean or proportion

**Value**
Data frame containing test result, p-value, critical values

## z_test_leq(H0, sigma = FALSE, alpha, samp_size, x_bar)
**Description**: Conducts lower-tailed z-test, equivalent of Ha < H0.

**Arguments** 
- H0: Null hyothesis
- sigma = (Optional) population standard deviation
- samp_size: Sample size
- x_bar: Sample mean or proportion

**Value**
Data frame containing test result, p-value, critical value

## z_test_geq(H0, sigma = FALSE, alpha, samp_size, x_bar)
**Description**: Conducts upper-tailed z-test, equivalent of Ha > H0

**Arguments** 
- H0: Null hyothesis
- sigma = (Optional) population standard deviation
- samp_size: Sample size
- x_bar: Sample mean or proportion

**Value**
Data frame containing test result, p-value, critical value

## get_p_value(z_score, two_sided = FALSE)
**Note: You should use a ``z_test()`` function instead to obtain the p-value if you are also looking for the z-test results.**

**Description**: Gets the p-value

**Arguments** 
- z_score: z-score
- two_sided: Boolean value denoting whether test is double sided or not, defaults to FALSE

**Value**
P-value

## get_z(p_hat, p_null, size, sigma = NULL)
**Note: You should use a ``z_test()`` function instead to obtain the z-score if you are also looking for the z-test results with the p-value.**

**Description**: Gets the z-score

**Arguments** 
- p_hat: Sample probability
- p_null: Null hypothesis value
- size: Sample size
- sigma: (Optional) standard deviation, defaults to NULL

**Value**
Z-score

## get_z_given_sd(p_hat, p_null, sd, size)
**Note: this function should NOT be used, use ``get_z()`` instead and pass the standard deviation as ``sigma``.**

**Description**: Gets the z-score given the standard deviation.

**Arguments** 
- p_hat: Sample probability
- p-null: Null hypothesis value
- sd: Standard deviation
- size: Sample size

**Value**
Z-score

## interval(trials, successes, conf)
**Description**: Gets the confidence interval.

**Arguments** 
- trials: Number of trials
- successes: Number of successes of the trials
- conf: Confidence level

**Value**
Confidence level formatted as a String

## type_2_err(H0, Ha, p, n, alpha, sigma = NULL)
**Description**: Gets the probability of making a Type II error. Equivalent to the probability of incorrectly failing to reject the null hypothesis. 

**Arguments** 
- H0: Null hypothesis
- Ha: Direction of the test, valid arguments are either "<", ">", or "!="
- mu_alt: True probability/mean value
- n: Sample size
- alpha: Significance level/Type I error probability/alpha level
- sigma (Optional) population standard deviation

**Value**
Type II error, or Beta
## type_2_err_min_size(alpha, beta, H0, Ha, two_sided = FALSE, sigma = NULL)
**Description**: Gets the minimum sample size required to obtain a desired probability of making a Type II error. 

**Arguments** 
- alpha: Probability of Type I error
- beta: Probability of Type II error desired
- H0: Null hypothesis
- Ha: Actual mean or probability used with beta
- two_sided: (Optional) Boolean value denoting whether test is double sided or not, defaults to FALSE
- sigma: (Optional) Standard deviation. Defaults to NULL, for when H0 and Ha are probabilities and not mean values

**Value**
NOT rounded minimum sample size needed (generally round upwards for an upper test)

## power(H0, Ha, p, n, alpha, sigma = NULL)
**Description**: Gets the probability of correctly rejecting the null hypothesis. Equivalent to (1 - Beta), or (1 - the probability of making a type II error).

**Arguments** 
- H0: Null hypothesis
- Ha: Direction of the test, valid arguments are either "<", ">", or "!="
- mu_alt: True probability/mean value
- n: Sample size
- alpha: Significance level/Type I error probability/alpha level
- sigma (Optional) population standard deviation

**Value**
Power, or 1 - Beta
