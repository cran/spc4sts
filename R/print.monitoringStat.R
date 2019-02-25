print.monitoringStat <- function(x, ...) {

  if (!is.null(x$localStat))
    cat('Local defect monitoring statistic:', x$localStat,'\n')

  if (!is.null(x$globalStat))
    cat('Global change monitoring statistic:', x$globalStat,'\n')

  invisible(x)
}
