#' Gets the degrees of freedom in 2-samp t-test
#'
#' This function takes in 2 vectors of a 2-sample t-test and
#' returns the degrees of freedom
#'
#' @author Timothy Roeper, PhD Professor at New York University
#' @param x data for sample 1
#' @param y data for sample 2
#' @return degrees of freedom
#' @export
approx_df_2_sample_t_test = function(x, y) {
  x = as_vector(x)
  y = as_vector(y)
  s2_x = var(x)
  s2_y = var(y)
  n_x = length(x)
  n_y = length(y)
  df_x = n_x - 1
  df_y = n_y - 1
  approx = (s2_x / n_x + s2_y / n_y)^2 /
    (s2_x^2 / (n_x^2 * df_x) + s2_y^2 / (n_y^2 * df_y))
  return(approx)
}
