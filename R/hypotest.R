#' Get Z score
#'
#' This function gets z-score given sample p (p_hat), null hypothesis value (p_null), and sample size
#'
#' @param p_hat Sample probability
#' @param p_null null hypothesis value
#' @param size Sample size
#' @param sigma (Optional) Standard deviation, defaults to NULL
#' @return Z-score
#' @export
get_z <- function(p_hat, p_null, size, sigma = NULL) {
  if(is.null(sigma)) {
    message("You have not set sigma value, it will default to NULL")
    numerator <- p_hat - p_null
    samp_sd <- sqrt(p_null * (1 - p_null) / size)
    return(numerator / samp_sd)
  } else {
    return(get_z_given_sd(p_hat = p_hat, p_null = p_null, sd = sigma, size = size))
  }
}

#' Gets Z-score given standard deviation. You can just call get_z() and pass in the standard deviation.
#'
#' gets z-score given sample p (p_hat), null hypothesis value (p_null), population standard deviation (sd), and sample
#'
#' @param p_hat Sample probability
#' @param p_sull Null hypothesis value
#' @param sd Standard deviation
#' @param size Sample size
#' @return Z-score
#' @export
get_z_given_sd <- function(p_hat, p_null, sd, size) {
  numerator = p_hat - p_null
  samp_sd = sd/sqrt(size)
  result <- numerator / samp_sd
}

#' Gets p-value
#'
#' usage: defaults to 1 sided test, call with parameter (doubleSided = TRUE) to get p-value of a double sided test
#'
#' @param z_score z-score
#' @param double_sided Boolean value denoting whether test is double sided or not, defaults to FALSE
#' @return p-value
#' @export
get_p_value <- function(z_score, double_sided = FALSE) {
  result <- if(double_sided) 2*(1-pnorm(abs(z_score))) else 1-pnorm(z_score)
}
