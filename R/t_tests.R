
#' Runs a 2-sample t test assuming equal variances
#' 
#' Given that the variances are equal, the degrees of freedom calculation is much simpler. This 
#' hypotest formula takes advantage of the it and performs a 2-sample t-test
#' 
#' @param samp1 data for the first sample
#' @param samp2 data for the second sample
#' @param H0_diff H0 value that tests the difference, defaults to 0 (testing if two samples have equal means
#' @param Ha direction of the test. Accepts either ">", "<", or "!="
#' @param alpha significance level, or the probability of making a type 1 error. Defaults to 0.05
#' @return data frame containing p-value and result of the hypothesis test
#' @export
two_samp_test_equal_var <- function(samp1, samp2, H0_diff = 0, Ha, alpha = 0.05) {
  samp1_mean <- mean(samp1)
  samp2_mean <- mean(samp2)
  n <- length(samp1)
  m <- length(samp2)
  pooled_var <- ((n-1) * var(samp1) + (m-1) * var(samp2)) / (n - 1 + m - 1)
  diff_var <- pooled_var * (1 / n + 1 / m)
  t_stat <- (samp1_mean - samp2_mean - H0_diff) / sqrt(diff_var)
  df <- n + m - 2
  if(Ha == "!=") {
    p_val <- 2 * (1 - pt(abs(t_stat), df = df))
    
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val
    ))
  }
  else if(Ha == ">") {
    p_val <- (1 - pt(abs(t_stat), df = df))
    
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val
    ))
  }
  else if(Ha == "<") {
    p_val <- (pt(t_stat, df = df))
    
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val
    ))
  }
  return(message("There was an error, Ha value not specified."))
}


#' 2-sample z-test with binomial distribution
#' 
#' Given that the samples use a binomial distribution, this function performs a 2-sample z-test
#' 
#' @param p1_trials number of trials in the first group
#' @param p1_successes number of successes in the first group
#' @param p2_trials number of trails in the second group
#' @param p2_successes number of successes in the second group
#' @param H0_diff H0 value that tests the difference, defaults to 0 (testing if two samples have equal means
#' @param Ha direction of the test. Accepts either ">", "<", or "!="
#' @param alpha significance level, or the probability of making a type 1 error. Defaults to 0.05
#' @return data frame containing p-value, z-score, and result of the hypothesis test
#' @export
two_samp_binom <- function(p1_trials, p1_successes, p2_trials, p2_successes, H0_diff = 0, Ha, alpha = 0.05) {
  p1 <- p1_successes / p1_trials  
  p2 <- p2_successes / p2_trials
  
  p = (p1_successes + p2_successes) / (p1_trials + p2_trials)
  
  var_d = p * (1 - p) * (1 / p1_trials + 1 / p2_trials)
  
  z_stat = (p1 - p2 - H0_diff) / sqrt(var_d)
  
  if(Ha == "!=") {
    p_val = 2 * (1 - pnorm(abs(z_stat)))
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "z-score" = z_stat))
  }
  else if(Ha == ">") {
    p_val = (1 - pnorm(abs(z_stat)))
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "z-score" = z_stat))
  }
  else if(Ha == "<") {
    p_val =  pnorm(z_stat)
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "z-score" = z_stat))
  }
  return(message("There was an error, Ha value not specified."))
}


#' Performs 2-sided 2-sample t-test
#' 
#' Tests the alternate hypothesis that states the H0 value is not H0_diff (usually 0), 
#' resulting in a 2-sided test
#' 
#' @param samp1_mean mean of the first group
#' @param samp2_mean mean of the first group
#' @param samp1_size mean of the first group
#' @param samp2_size mean of the first group
#' @param s1 sample standard deviation of the first group
#' @param s2 sample standard deviation of the second group
#' @param H0_diff H0 value that tests the difference, defaults to 0 (testing if two samples have equal means
#' @param Ha direction of the test. Accepts either ">", "<", or "!="
#' @param alpha significance level, or the probability of making a type 1 error. Defaults to 0.05
#' @param df degrees of freedom, defaults to NULL (program will calculate it if not specified)
#' @return data frame containing p-value, z-score, and result of the hypothesis test
#' @export
two_samp_t_test_neq <- function(samp1_mean, samp2_mean, samp1_size, samp2_size, s1, s2, H0_diff = 0, alpha = 0.05, df = NULL) {
  diff = samp1_mean - samp2_mean
  diff_var = sqrt(s1^2 / samp1_size + s2^2 / samp2_size)
  s2_x = s1^2
  s2_y = s2^2
  n_x = samp1_size
  n_y = samp2_size
  df_x = n_x - 1
  df_y = n_y - 1
  
  if(is.null(df)) {
    df = (s2_x / n_x + s2_y / n_y)^2 /
      (s2_x^2 / (n_x^2 * df_x) + s2_y^2 / (n_y^2 * df_y))
    t_stat = diff / diff_var
    p_val = (1 - pt(abs(t_stat), df = df)) + pt(t_stat, df = df)
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "t-score" = t_stat))
  }
  t_stat = diff / diff_var
  p_val = (1 - pt(abs(t_stat), df = df)) + pt(t_stat, df = df)
  return(data.frame("Reject H0" = p_val < alpha,
                    "p-value" = p_val,
                    "t-score" = t_stat))
  
}


#' Performs lower-tailed 2-sample t-test
#' 
#' Tests the alternate hypothesis that states the H0 value is less than H0_diff (usually 0), 
#' resulting in a lower-tailed 1-sided
#' 
#' @param samp1_mean mean of the first group
#' @param samp2_mean mean of the first group
#' @param samp1_size mean of the first group
#' @param samp2_size mean of the first group
#' @param s1 sample standard deviation of the first group
#' @param s2 sample standard deviation of the second group
#' @param H0_diff H0 value that tests the difference, defaults to 0 (testing if two samples have equal means
#' @param Ha direction of the test. Accepts either ">", "<", or "!="
#' @param alpha significance level, or the probability of making a type 1 error. Defaults to 0.05
#' @param df degrees of freedom, defaults to NULL (program will calculate it if not specified)
#' @return data frame containing p-value, z-score, and result of the hypothesis test
#' @export
two_samp_t_test_leq <- function(samp1_mean, samp2_mean, samp1_size, samp2_size, s1, s2, H0_diff = 0, alpha = 0.05, df = NULL) {
  diff = samp1_mean - samp2_mean
  diff_var = sqrt(s1^2 / samp1_size + s2^2 / samp2_size)
  s2_x = s1^2
  s2_y = s2^2
  n_x = samp1_size
  n_y = samp2_size
  df_x = n_x - 1
  df_y = n_y - 1
  
  if(is.null(df)) {
    df = (s2_x / n_x + s2_y / n_y)^2 /
      (s2_x^2 / (n_x^2 * df_x) + s2_y^2 / (n_y^2 * df_y))
    t_stat = diff / diff_var
    p_val = (pt(-abs(t_stat), df = df))
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "t-score" = t_stat))
  }
  t_stat = diff / diff_var
  p_val = (pt(-abs(t_stat), df = df))
  return(data.frame("Reject H0" = p_val < alpha,
                    "p-value" = p_val,
                    "t-score" = t_stat))
  
}


#' Performs upper-tailed 2-sample t-test
#' 
#' Tests the alternate hypothesis that states the H0 value is greater than H0_diff (usually 0), 
#' resulting in a upper-tailed 1-sided
#' 
#' @param samp1_mean mean of the first group
#' @param samp2_mean mean of the first group
#' @param samp1_size mean of the first group
#' @param samp2_size mean of the first group
#' @param s1 sample standard deviation of the first group
#' @param s2 sample standard deviation of the second group
#' @param H0_diff H0 value that tests the difference, defaults to 0 (testing if two samples have equal means
#' @param Ha direction of the test. Accepts either ">", "<", or "!="
#' @param alpha significance level, or the probability of making a type 1 error. Defaults to 0.05
#' @param df degrees of freedom, defaults to NULL (program will calculate it if not specified)
#' @return data frame containing p-value, z-score, and result of the hypothesis test
#' @export
two_samp_t_test_leq <- function(samp1_mean, samp2_mean, samp1_size, samp2_size, s1, s2, H0_diff = 0, alpha = 0.05, df = NULL) {
  diff = samp1_mean - samp2_mean
  diff_var = sqrt(s1^2 / samp1_size + s2^2 / samp2_size)
  s2_x = s1^2
  s2_y = s2^2
  n_x = samp1_size
  n_y = samp2_size
  df_x = n_x - 1
  df_y = n_y - 1
  
  if(is.null(df)) {
    df = (s2_x / n_x + s2_y / n_y)^2 /
      (s2_x^2 / (n_x^2 * df_x) + s2_y^2 / (n_y^2 * df_y))
    t_stat = diff / diff_var
    p_val = 1 - (pt(abs(t_stat), df = df))
    return(data.frame("Reject H0" = p_val < alpha,
                      "p-value" = p_val,
                      "t-score" = t_stat))
  }
  t_stat = diff / diff_var
  p_val = 1 - (pt(abs(t_stat), df = df))
  return(data.frame("Reject H0" = p_val < alpha,
                    "p-value" = p_val,
                    "t-score" = t_stat))
  
}






