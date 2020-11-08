#' Generates probability of Type II error
#'
#' This function generates the probability of having a type II error given
#' the null hypothesis, what the alternate hypothesis tests (higher, lower, or not equals to),
#' the actual probability/mean, sample size, and significance level alpha
#'
#' @param H0 Null hypothesis
#' @param Ha Direction of the test, valid arguments are either "<", ">", or "!="
#' @param mu_alt True mean or probability
#' @param n Sample size
#' @param alpha Significance level/Type I error probability/alpha level
#' @param sigma (Optional) population standard deviation
#' @return probability of Type II error
#' @export
type_2_err <- function(H0, Ha, mu_alt, n, alpha, sigma = NULL) {
  if(is.null(sigma)) {
    message("You have not specified an optional population sd. This is usually when working with probabilities.")
    # 1-sided test with Ha = greater than some value
    if(Ha == ">") {
      z = qnorm(1 - alpha)

      sd = sqrt(H0 * (1 - H0) / n)
      upper = H0 + z * sd

      B = pnorm((upper - mu_alt) / sqrt(mu_alt * (1 - mu_alt) / n))
      return(B)
    }
    else if(Ha == "<") {
      z = qnorm(1 - alpha)

      sd = sqrt(H0 * (1 - H0) / n)
      lower = H0 - z * sd

      z_less = pnorm((lower - mu_alt) / sqrt(mu_alt * (1 - mu_alt) / n))
      B = 1 - z_less
      return(B)
    }
    else if(Ha == "!=") {
      z = qnorm(1 - (alpha) / 2)

      sd = sqrt(H0 * (1 - H0) / n)
      lower = H0 - z * sd
      upper = H0 + z * sd

      z_less = pnorm((lower - mu_alt) / sqrt(mu_alt * (1 - mu_alt) / n))
      z_greater = 1 - pnorm((upper - mu_alt) / sqrt(mu_alt * (1 - mu_alt) / n))

      B = z_greater - z_less
      return(B)
    }
    else {
      return(cat(paste("Usage: please specify a valid Ha value\nOptions: Ha = \"<\", \">\", \"!=\"")))
    }
  }

  # sigma given, working with numerical values
  else {
    if(Ha == ">") {
      z = qnorm(1 - alpha)

      sd = sigma/sqrt(n)
      upper = H0 + z * sd

      B = pnorm((upper - mu_alt) / sd)
      return(B)
    }
    else if(Ha == "<") {
      z = qnorm(1 - alpha)

      sd = sigma/sqrt(n)
      lower = H0 - z * sd

      z_less = pnorm((lower - mu_alt) / sd)
      B = 1 - z_less
      return(B)
    }
    else if(Ha == "!=") {
      z = qnorm(1 - (alpha)/2)

      sd = sigma/sqrt(n)
      lower = H0 - z * sd
      upper = H0 + z * sd

      z_less = pnorm((lower - mu_alt) / sd)
      z_greater = pnorm((upper - mu_alt) / sd)

      B = z_greater - z_less
      return(B)
    }
    else {
      return(cat(paste("Usage: please specify a valid Ha value\nOptions: Ha = \"<\", \">\", \"!=\"")))
    }
  }
}

#' Gets minimum sample size needed for certain Beta (Type II error) value
#'
#' This function will find the minimum samples needed to obtain a desired Type II error, or beta.
#' It takes in the significance level, desired Type II error, null hypothesis, actual mean/probability.
#'
#' @param alpha Probability of Type I error
#' @param beta Probability of Type II error
#' @param H0 Null hypothesis
#' @param Ha Actual mean or probability used with beta
#' @param two_sided (Optional) Boolean to determine if it is a 2-sided test or not. Defaults to 1-sided test
#' @param sigma (Optional) Standard deviation. Defaults to NULL, for when H0 and Ha are probabilities and not mean values
#' @return NOT rounded minimum sample size needed (generally round upwards for an upper test)
#' @export
type_2_err_min_size <- function(alpha, beta, H0, Ha, two_sided = FALSE, sigma = NULL) {
  if(!two_sided) {
    message("You haven't specified a 2 or 1 sided test. The program defaults to a 1 sided test.")
  }
  if(is.null(sigma)) {
    message("You have not specified a sd, this is when H0 and Ha are probabilities instead of numerical quantities.")
    z_alpha = if(two_sided) qnorm(1 - (alpha/2)) else qnorm(1 - alpha)
    z_beta = qnorm(1 - beta)

    n = (((z_beta * sqrt(Ha * (1 - Ha))) + (z_alpha * sqrt(H0 * (1 - H0))))/(H0 - Ha))^2
    return(n)
  }
  else if(!is.null(sigma)){
    z_alpha = if(two_sided) qnorm(1 - (alpha/2)) else qnorm(1 - alpha)
    z_beta = qnorm(1 - beta)

    n = ((z_alpha + z_beta)^2 * (sigma^2))/((H0 - Ha)^2)
    return(n)
  }
}
