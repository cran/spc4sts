climit2 <- function(object, grate, nD) {

  if (!missing(grate))   object$control.limit <- quantile(object$PIstats, 1-grate)
  if (!missing(nD)) {
    nD <- nD*length(object$PIstats)
    object$diagnostic.threshold <- (object$nDmaxSms[nD] + object$nDmaxSms[nD+1])/2
  }
  object
}
