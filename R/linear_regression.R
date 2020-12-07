#' Returns linear regression model (least squares), sum of residuals squared, total resioduals squared,
#' y-hat predicted values, residuals squared, total squares given vectors x, y
#' 
#' @param vector x for independent variable values
#' @param vector y for dependent varaible values
#' 
#' @return data frame containing linear regression results, including R^2.
#' @export 
linear_regression = function(x, y) {
  beta_1_hat <- cov(x, y) / var(x)
  beta_0_hat <- mean(y) - beta_1_hat * mean(x)
  y_hat = beta_0_hat + x * beta_1_hat
  
  residuals_sq = (y - y_hat)^2
  SSR = sum(residuals)
  
  squares = (y - mean(y))^2
  TSS = sum(squares)
  
  R_sq = 1 - (SSR / TSS)
  
  return (data.frame("R_sq" = R_sq, 
                     "SSR" = SSR,
                     "TSS" = TSS,
                     "x" = x,
                     "y" = y,
                     "y-hat" = y_hat,
                     "resid_sq" = residuals_sq,
                     "total squares" = squares))
  
}