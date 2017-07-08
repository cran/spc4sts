climit <- function(imgs, model, stat, w, grate = c(.05,.0027), nD = seq(5, 100, length.out = 10)) {

  if (missing(stat)) stat = "bp"
  N <- dim(imgs)[3]
  M <- dim(imgs)[1]*dim(imgs)[2]
  nD <- nD*N
  nD.max = max(nD)
  tmp <- NULL
  PIstats <- vector("numeric", N)
  for (j in 1:N) {
    ls <- localStat(imgs[,,j], model, stat, w)
    tmp <- sort(c(tmp, ls$sms), decreasing=T)[1:(nD.max+1)]
    PIstats[j] <- ls$monitoringStat
  }
  dth <- (tmp[nD] + tmp[nD+1])/2

  list(nDmaxSms = tmp,
       PIstats = PIstats,
       diagnostic.threshold = dth,
       stat = stat,
       w = w,
       control.limit = quantile(PIstats,  1 - grate)
       )
}
