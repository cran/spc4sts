climit2 <- function(cl, fa.rate, nD) {

  if (!inherits(cl, "climit"))
    stop("cl must be an object returned by the climit or climit2 functions!")

  if (!missing(fa.rate)) {
    if (!is.null(cl$localStat))
      cl$localStat$control.limit <- quantile(cl$localStat$PIstats, 1 - fa.rate)
    if (!is.null(cl$globalStat)) {
      k1 <- mean(cl$globalStat$PIstats)
      k2 <- var(cl$globalStat$PIstats)
      k3 <- mean((cl$globalStat$PIstats - k1)^3)
      k <- 8*(k2^3)/(k3^2)
      b <- k3/(4*k2)
      a <- k1 - 2*(k2^2)/k3
      if (b >=0)
        cl$globalStat$control.limit.trans_chi2 <- qchisq(1 - fa.rate, k)*b + a
      else
        cl$globalStat$control.limit.trans_chi2 <- qchisq(fa.rate, k)*b + a
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
