climit <- function(imgs, model, stat = c("ad", "bp"), w, grate = c(.05,.0027), nD = c(5, 100)) {

  if (!is.array(imgs)) stop("img must be a 3-dimensional array.")
  else if (length(dim(imgs)) != 3) stop("img must be a 3-dimensional array.")

  if (class(model) != "surfacemodel") stop("Wrong input for the model argument! Needs a surfacemodel object.")

  if (missing(w)) stop("Missing argument w. It must be an odd number >= 3.")
  else if (w < 3 || w %% 2 < 1) stop("w must be an odd number >= 3.")

  stat <- match.arg(stat)

  N <- dim(imgs)[3]
  M <- dim(imgs)[1]*dim(imgs)[2]
  nD <- nD*N
  nD.max = max(nD)
  tmp <- NULL
  PIstats <- vector("numeric", N)
  for (j in 1:N) {
    ls <- localStat(imgs[,,j], model, stat = stat, w = w)
    tmp <- sort(c(tmp, ls$sms), decreasing=T)[1:(nD.max+1)]
    PIstats[j] <- ls$monitoringStat
  }
  dth <- (tmp[nD] + tmp[nD+1])/2

  out <- list(nDmaxSms = tmp,
       PIstats = PIstats,
       diagnostic.threshold = dth,
       stat = stat,
       w = w,
       control.limit = quantile(PIstats,  1 - grate)
       )
  class(out) <- "climit"
  out
}
