#' Generates probability of Type II error
#' 
#' This function generates the probability of having a type II error given
#' the null hypothesis, what the alternate hypothesis tests (higher, lower, or not equals to), 
#' the actual probability/mean, sample size, and significance level alpha
#' 
#' @param Ho Null hypothesis
#' @param Ha Direction of the test, valid arguments are either "<", ">", or "!="
#' @param p True probability
#' @param n Sample size
#' @param alpha Significance level/Type I error probability/alpha level
#' @return probability of Type II error
#' @export
type2err <- function(Ho, Ha, p, n, alpha ) {
  # 1-sided test with Ha = greater than some value
  if(Ha == ">") {
    z = qnorm(1 - alpha)
    
    sd = sqrt(Ho * (1 - Ho)/n)
    upper = Ho + z * sd
    
    zGreater = 1 - pnorm((upper - p)/sqrt(p * (1 - p)/n))
    B = 1 - zGreater
    return(B)
  }
  else if(Ha == "<") {
    z = qnorm(1 - alpha)
    
    sd = sqrt(Ho * (1 - Ho)/n)
    lower = Ho - z * sd
    
    zLess = pnorm((lower - p)/sqrt(p * (1 - p)/n))
    B = 1 - zLess
    return(B)
  }
  else if(Ha == "!=") {
    z = qnorm(1 - (alpha)/2)
    
    sd = sqrt(Ho * (1 - Ho)/n)
    lower = Ho - z * sd
    upper = Ho + z * sd
    
    zLess = pnorm((lower - p)/sqrt(p * (1 - p)/n))
    zGreater = 1 - pnorm((upper - p)/sqrt(p * (1 - p)/n))
    
    B = 1 - (zLess + zGreater)
    return(B)
  }
  else {
    return(cat(paste("Usage: please specify a valid Ha value\nOptions: Ha = \"<\", \">\", \"!=\"")))
  }
}
