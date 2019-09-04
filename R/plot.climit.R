plot.climit <- function(x, ...) {

  if (!is.null(x$localStat)) {
      statistics <- x$localStat$PIstats
      hist(statistics,
           main='',
           prob=TRUE, ...)
      abline(v=x$localStat$control.limit, lty=2)
  }
  if (!is.null(x$globalStat)) {
      statistics <- x$globalStat$PIstats
      hist(statistics, main='', prob=TRUE, ...)
      v <- c(x$globalStat$control.limit.ecdf,
             x$globalStat$control.limit.trans_chi2)
      abline(v=v[1], lty=2)
      abline(v=v[2], lty=3)
      legend("topright", legend=c('ecdf', 'chi2'), lty=2:3)
  }
}
