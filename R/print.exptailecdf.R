print.exptailecdf <- function(x, ...) {

  cat('Exponential-tail empirical CDF:\n')
  cat('Left tail (<=', x$joint[1])
  cat('): lambda =', x$lambda[1])

  cat('\nRight tail (>=', x$joint[2])
  cat('): lambda =', x$lambda[2])
  cat('\nIn-between: use the ')
  print(x$ecdf)
  invisible(x)
}
