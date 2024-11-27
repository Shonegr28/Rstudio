#' Title
#'
#' @param d
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
myci = function(d, alpha = 0.05) {
  meanin = mean(d)
  std = sd(d)
  num = length(d)

  # Calculate the t value for alpha/2
  t = qt(p = 1 - alpha / 2, df = num - 1)

  # Initialize the confidence interval vector
  ci = numeric(2)
  ci[1] = meanin - t * std / sqrt(num)
  ci[2] = meanin + t * std / sqrt(num)

  # Print the lower and upper bounds
  cat("Lower Bound:", ci[1], "Upper Bound:", ci[2], "\n")

  # Return the intervals
  return(ci)
}
