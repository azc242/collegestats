#' Gets powe, or the probability of correctly rejecting the null hypothesis.
#' 
#' This function will return the probability of rejecting the null hypothesis
#' when it is in fact false, given the null hypothesis value, direction of the 
#' test, alternative mu value, sample size, significane level alpha, and
#' (optionally) the population standard deviation
#' 
#' @param Ho Null hypothesis
#' @param Ha Direction of the test, valid arguments are either "<", ">", or "!="
#' @param mu_alt True mean or probability
#' @param n Sample size
#' @param alpha Significance level/Type I error probability/alpha level
#' @param sigma (Optional) population standard deviation
#' @return probability of Type II error
#' @export
power <- function(Ho, Ha, mu_alt, n, alpha, sigma = NULL) {
  return(1 - type_2_err(Ho = Ho, Ha = Ha, mu_alt = mu_alt, n = n, alpha = alpha, sigma = sigma))
}
