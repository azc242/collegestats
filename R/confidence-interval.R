
#' Get confidence interval
#'
#' This function gets the confidence interval given the
#' number of trials, number of successes, and confidence level.
#' Confidence level should be given in decimal form, not percentage.
#'
#' @param trials Number of trials
#' @param successes Number of successes of the trials
#' @param conf Confidence level
#' @return Confidence level formatted as a String
#' @export
# returns confidence interval given number of trails, successes, and confidence level
interval <- function(trials, successes, conf) {
  temp = 1 - (1 - conf)/2
  z = qnorm(temp)
  n = trials
  success = success
  p = success / n

  #getting the var and SD of the estimator
  var = (p * (1-p))/n
  sd = sqrt(var)

  # final interval
  lower = p - (sd*z)
  upper = p + (sd*z)

  result = paste("(", p - (sd*z), ",", p + (sd*z),")" )
}
