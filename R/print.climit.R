print.climit <- function(x, ...) {

  if (1 %in% x$type) {
    cat('Local defect:\n')
    cat('    Spatial moving window statistic:', x$localStat$stat, '\n')
    cat('    Spatial moving window size:', x$localStat$w, '\n')
    cat('    Target false alarm rate:', x$fa.rate, '\n')
    cat('    Control limit:', x$localStat$control.limit, '\n')
    cat('    Diagnostic threshold:', x$localStat$control.limit, '\n')
  }

  if (2 %in% x$type) {
    cat('Global change:\n')
    cat('    Target false alarm rate:', x$fa.rate, '\n')
    cat('    Transformed chi-squared control limit:', x$globalStat$control.limit.trans_chi2, '\n')
    cat('    Empirical control limit:', x$globalStat$control.limit.ecdf, '\n')
  }

  invisible(x)
}
