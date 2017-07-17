climit2 <- function(cl, grate, nD) {

  if (class(cl) != "climit")
    stop("cl must be an object returned by the climit or climit2 functions!")
  if (!missing(grate))
    cl$control.limit <- quantile(cl$PIstats, 1-grate)
  if (!missing(nD)) {
    nD <- nD*length(cl$PIstats)
    cl$diagnostic.threshold <- (cl$nDmaxSms[nD] + cl$nDmaxSms[nD+1])/2
  }
  cl
}
