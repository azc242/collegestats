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
#' @param sigma (Optional) population standard deviation
#' @return probability of Type II error
#' @export
#' Ha takes in either ">", "<", "!="
type2err <- function(Ho, Ha, p, n, alpha, sigma = NULL) {
  if(is.null(sigma)) {
    message("You have not specified an optional population sd. This is usually when working with probabilities.")
    # 1-sided test with Ha = greater than some value
    if(Ha == ">") {
      z = qnorm(1 - alpha)
      
      sd = sqrt(Ho * (1 - Ho)/n)
      upper = Ho + z * sd
      
      B = pnorm((upper - p)/sqrt(p * (1 - p)/n))
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
      
      B = zGreater - zLess
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
      upper = Ho + z * sd
      
      B = pnorm((upper - p)/sd)
      return(B)
    }
    else if(Ha == "<") {
      z = qnorm(1 - alpha)
      
      sd = sigma/sqrt(n)
      lower = Ho - z * sd
      
      zLess = pnorm((lower - p)/sd)
      B = 1 - zLess
      return(B)
    }
    else if(Ha == "!=") {
      z = qnorm(1 - (alpha)/2)
      
      sd = sigma/sqrt(n)
      lower = Ho - z * sd
      upper = Ho + z * sd
      
      zLess = pnorm((lower - p)/sqrt(p * (1 - p)/n))
      zGreater = pnorm((upper - p)/sd)
      
      B = zGreater - zLess
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
#' @param Ho Null hypothesis 
#' @param Ha Actual mean or probability used with beta
#' @param twoSided (Optional) Boolean to determine if it is a 2-sided test or not. Defaults to 1-sided test
#' @param sigma (Optional) Standard deviation. Defaults to NULL, for when Ho and Ha are probabilities and not mean values
#' @return NOT rounded minimum sample size needed (generally round upwards)
#' @export
minSampSizeType2 <- function(alpha, beta, Ho, Ha, twoSided = FALSE, sigma = NULL) {
  if(!twoSided) {
    message("You haven't specified a 2 or 1 sided test. The program defaults to a 1 sided test.")
  }
  if(is.null(sigma)) {
    message("You have not specified a sd, this is when Ho and Ha are probabilities instead of numerical quantities.")
    z_alpha = if(twoSided) qnorm(1 - (alpha/2)) else qnorm(1 - alpha)
    z_beta = qnorm(1 - beta)
    
    n = (((z_beta * sqrt(Ha * (1 - Ha))) + (z_alpha * sqrt(Ho * (1 - Ho))))/(Ho - Ha))^2
    return(n)
  }
  else if(!is.null(sigma)){
    z_alpha = if(twoSided) qnorm(1 - (alpha/2)) else qnorm(1 - alpha)
    z_beta = qnorm(1 - beta)
    
    n = ((z_alpha + z_beta)^2 * (sigma^2))/((Ho - Ha)^2)
    return(n)
  }
}