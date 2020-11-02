#' Get Z score
#'
#' This function gets z-score given sample p (pHat), null hypothesis value (pNull), and sample size
#'
#' @param pHat Sample probability
#' @param pNull null hypothesis value
#' @param size Sample size
#' @return Z-score
#' @export
getZ <- function(pHat, pNull, size) {
  numerator = pHat - pNull
  sampSD = sqrt(pNull*(1-pNull)/size)
  result <- numerator/sampSD
}

#' Gets Z-score given standard deviation
#'
#' gets z-score given sample p (pHat), null hypothesis value (pNull), population standard deviation (sd), and sample
#'
#' @param pHat Sample probability
#' @param pNull null hypothesis value
#' @param sd Standard deviation
#' @param size Sample size
#' @return Z-score
#' @export
getZGivenSD <- function(pHat, pNull, sd, size) {
  numerator = pHat - pNull
  sampSD = sd/sqrt(size)
  result <- numerator/sampSD
}

#' Gets p-value
#'
#' usage: defaults to 1 sided test, call with parameter (doubleSided = TRUE) to get p-value of a double sided test
#'
#' @param zScore z-score
#' @param doubleSided Boolean value denoting whether test is double sided or not, defaults to FALSE
#' @return p-value
#' @export
getP <- function(zScore, doubleSided = FALSE) {
  result <- if(doubleSided) 2*(1-pnorm(abs(zScore))) else 1-pnorm(zScore)
}
