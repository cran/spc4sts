ad <- function(r, P) {

  n <- length(r)

  P <- c(P)[order(c(r))]

  P_rev <- 1 - rev(P)

  -n - sum(seq(1, 2*n, 2)*log(P * P_rev))/n

}
