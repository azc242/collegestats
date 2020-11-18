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
#' Gets z-score given sample p (p_hat), null hypothesis value (p_null), population standard deviation (sd), and sample
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
#' Defaults to 1 sided test, call with parameter (doubleSided = TRUE) to get p-value of a double sided test
#'
#' @param z_score Z-score
#' @param two_sided Boolean value denoting whether test is double sided or not, defaults to FALSE
#' @return p-value
#' @export
get_p_value <- function(z_score, two_sided = FALSE) {
  result <- if(two_sided) 2*(1-pnorm(abs(z_score))) else 1-pnorm(z_score)
}

#' Conducts double sided z-test
#'
#' Gets the outcome, p-value, and upper critical value of a double-sided hypothesis test (z-test)
#' given the null hypothesis, population standard deviation, significance leve, sample size, and sample mean
#'
#' @param H0 Null hypothesis
#' @param sigma (Optional) Population standard deviation, defaults to NULL
#' @param alpha Significance level, defaults to 0.05 or 5%
#' @param samp_size Sample size
#' @param x_bar Sample mean or proportion
#' @return data frame containing test result, p-value, and critical values
#' @export
z_test_neq <- function(H0, sigma = NULL, alpha = .05, samp_size, x_bar) {
  if(is.null(sigma)) {
    samp_sd <- sqrt(H0 * (1 - H0) / samp_size)
    upper_crit_value <- H0 + qnorm(1 - alpha/2) * samp_sd
    lower_crit_value <- H0 + qnorm(alpha / 2) * samp_sd
    reject_H0 = (x_bar < lower_crit_value | x_bar > upper_crit_value)
    p_value = 2 * (1 - pnorm(abs(x_bar - H0) / samp_sd))

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Upper critical value" = upper_crit_value,
                      "Lower critical value" = lower_crit_value
    ))
  }
  else {
    samp_sd <- sqrt(sigma^2 / samp_size)
    upper_crit_value <- H0 + qnorm(1 - alpha/2) * samp_sd
    lower_crit_value <- H0 + qnorm(alpha / 2) * samp_sd
    reject_H0 = (x_bar < lower_crit_value | x_bar > upper_crit_value)
    p_value = 2 * (1 - pnorm(abs(x_bar - H0) / samp_sd))

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Upper critical value" = upper_crit_value,
                      "Lower critical value" = lower_crit_value
    ))
  }
}

#' Conducts lower-tailed z-test
#'
#' Gets the outcome, p-value, and upper critical value of a lower-tailed hypothesis test (z-test)
#' given the null hypothesis, population standard deviation, significance leve, sample size, and sample mean
#'
#' @param H0 Null hypothesis
#' @param sigma (Optional) Population standard deviation, defaults to NULL
#' @param alpha Significance level, defaults to 0.05 or 5%
#' @param samp_size Sample size
#' @param x_bar Sample mean or proportion
#' @return data frame containing test result, p-value, and critical value
#' @export
z_test_leq <- function(H0, sigma = NULL, alpha = .05, samp_size, x_bar) {
  if(is.null(sigma)) {
    samp_sd <- sqrt(H0 * (1 - H0) / samp_size)
    lower_crit_value <- H0 + qnorm(alpha / 2) * samp_sd
    reject_H0 = (x_bar < lower_crit_value)
    p_value = 1 - pnorm(abs(x_bar - H0) / samp_sd)

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Lower critical value" = lower_crit_value
    ))
  }
  else {
    samp_sd <- sqrt(sigma^2 / samp_size)
    lower_crit_value <- H0 + qnorm(alpha / 2) * samp_sd
    reject_H0 = (x_bar < lower_crit_value)
    p_value = 1 - pnorm(abs(x_bar - H0)/samp_sd)

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Lower critical value" = lower_crit_value
    ))
  }
}

#' Conducts upper-tailed z-test
#'
#' Gets the outcome, p-value, and upper critical value of a upper-tailed hypothesis test (z-test)
#' given the null hypothesis, population standard deviation, significance leve, sample size, and sample mean
#'
#' @param H0 Null hypothesis
#' @param sigma (Optional) Population standard deviation, defaults to NULL
#' @param alpha Significance level, defaults to 0.05 or 5%
#' @param samp_size Sample size
#' @param x_bar Sample mean or proportion
#' @return data frame containing test result, p-value, and critical value
#' @export
z_test_geq <- function(H0, sigma = NULL, alpha = .05, samp_size, x_bar) {
  if(is.null(sigma)) {
    samp_sd <- sqrt(H0 * (1 - H0) / samp_size)
    upper_crit_value <- H0 + qnorm(1 - alpha/2) * samp_sd
    reject_H0 = (x_bar > upper_crit_value)
    print(abs(x_bar - H0) / samp_sd)
    p_value = 1 - pnorm(abs(x_bar - H0) / samp_sd)

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Upper critical value" = upper_crit_value
    ))
  }
  else {
    samp_sd <- sqrt(sigma^2 / samp_size)
    upper_crit_value <- H0 + qnorm(1 - alpha/2) * samp_sd
    reject_H0 = (x_bar > upper_crit_value)
    p_value = 1 - pnorm(abs(x_bar - H0)/samp_sd)

    return(data.frame("Reject H0" = reject_H0,
                      "p-value" = p_value,
                      "Upper critical value" = upper_crit_value
    ))
  }
}
