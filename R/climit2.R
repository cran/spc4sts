climit2 <- function(cl, fa.rate, nD) {

  if (class(cl) != "climit")
    stop("cl must be an object returned by the climit or climit2 functions!")

  if (!missing(fa.rate)) {
    if (!is.null(cl$localStat))
      cl$localStat$control.limit <- quantile(cl$localStat$PIstats, 1 - fa.rate)
    if (!is.null(cl$globalStat)) {
      cl$globalStat$control.limit.trans_chi2 <-
        qchisq(1 - fa.rate, cl$globalStat$k)*cl$globalStat$b + cl$globalStat$a
      cl$globalStat$control.limit.ecdf <- quantile(cl$globalStat$PIstats, 1 - fa.rate)
      names(cl$globalStat$control.limit.trans_chi2) <- names(cl$globalStat$control.limit.ecdf)
    }
  }

  if (!missing(nD)) {
    nD <- nD*length(cl$localStat$PIstats)
    cl$localStat$diagnostic.threshold <- (cl$localStat$nDmaxSms[nD] + cl$localStat$nDmaxSms[nD+1])/2
  }

  cl
}
