ntickets <- function(N, gamma, p){
  # N is Number of Seats
  # gamma is the probability a plane will be truly overbooked (more people show than there are seats)
  # P is probability of a "show"

  # calculates the number of tickets to be sold by discrete distribution
  numsold = (2 * N*(2*(1 - p)))
  sold <- round(numsold)

  showup <- qbinom(1 - gamma, sold, p)

  space <- subset(seq_along(showup), showup <= N)
  nd = tail(numsold[space], 1)


  x_function = function(n){
    mean_value = n * p
    std_dev = sqrt(n * p * (1 - p))
    quantile_value = qnorm(1 - gamma, mean_value, std_dev)
    num = quantile_value - 0.5
    return(num)
  }

  zero <- function(x) {
    qnorm_value <- x_function(x)
    difference <- qnorm_value - N
    return(difference)
  }
  lower_bound = N - N * 0.1
  upper_bound = N + N * 0.1
  nc = uniroot(zero, lower = lower_bound, upper = upper_bound)$root

  vprobs = 1 - pbinom(N, size = numsold, prob = p)

  plot(numsold, vprobs - gamma, type = "b", col = "black", lwd = 2, pch = 16,
       main = paste0("Objective Vs n to find optimal tickets sold", '\n', "(", nd, ") gamma= ", gamma, " N=", N, " discrete"),
       xlab = "n", ylab = "Objective")

  abline(v = nd, col = "red")
  abline(h = 0, col = "blue")

  pnorm_function <- function(n) {
    num <- 1 - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
    return(num)
  }

  curve((pnorm_function(x) - gamma), from = N, to = (N + N * (2 * (1 - p))), col = "black", lwd = 2,
        main = paste0("Objective Vs n to find optimal tickets sold", '\n', "(", round(nc, 5), ") gamma= ", gamma, " N=", N, " continuous"),
        xlab = "n", ylab = "Objective")

  abline(v = nc, col = "red")
  abline(h = 0, col = "blue")

  result <- list(nc = nc, nd = nd, N = N, p = p, gamma = gamma)

  print(result)
}

