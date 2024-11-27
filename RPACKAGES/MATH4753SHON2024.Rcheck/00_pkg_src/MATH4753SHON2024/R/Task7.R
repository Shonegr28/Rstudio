#' @title myncurve
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
myncurve <- function(a, mu, sigma) {
  # Calculate the cumulative probability from -∞ to a
  area <- pnorm(a, mean = mu, sd = sigma)

  # Plotting the normal distribution curve
  curve(dnorm(x, mean = mu, sd = sigma), from = mu - 3*sigma, to = mu + 3*sigma,
        col = "blue", lwd = 2, ylab = "Density", xlab = "x")

  # Shade the area under the curve from -∞ to a
  xcurve <- seq(mu - 3*sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col = "lightblue")

  # Return the calculated area
  return(area)
}
