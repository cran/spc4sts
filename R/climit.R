climit <- function(imgs, fa.rate = c(.05,.0027), model,
                   type, stat = c("ad", "bp"), w, nD = c(5, 100))
{

  if (!is.array(imgs)) stop("img must be a 3-dimensional array.")
  else if (length(dim(imgs)) != 3) stop("img must be a 3-dimensional array.")

  if (class(model) != "surfacemodel") stop("Wrong input for the model argument! Needs a surfacemodel object.")

  if (missing(type) | length(type) > 2 | !all(type %in% 1:2))
    stop("Missing type of monitoring statistic.
           It must be 1 (for local defect), 2 (for global change), or 1:2 (for both).")

  N <- dim(imgs)[3]

  if (1 %in% type) {
    if (missing(w)) stop("Missing argument w. It must be an odd number >= 3.")
    else if (w < 3 || w %% 2 < 1) stop("w must be an odd number >= 3.")
    stat <- match.arg(stat)

    M <- dim(imgs)[1]*dim(imgs)[2]
    nD <- nD*N
    nD.max = max(nD)
    tmp <- NULL
  }

  PI.local.stats <- PI.global.stats <- vector("numeric", N)
  for (j in 1:N) {
    mStat <- monitoringStat(img = imgs[,,j], model = model, type = type,
                            stat = stat, w = w)
    if (1 %in% type) {
      tmp <- sort(c(tmp, mStat$sms), decreasing=T)[1:(nD.max+1)]
      PI.local.stats[j] <- mStat$localStat
    }
    if (2 %in% type)
      PI.global.stats[j] <- mStat$globalStat
  }

  if (1 %in% type) {
    localStat <- list(nDmaxSms = tmp,
                      PIstats = PI.local.stats,
                      diagnostic.threshold = (tmp[nD] + tmp[nD+1])/2,
                      stat = stat,
                      w = w,
                      control.limit = quantile(PI.local.stats,  1 - fa.rate)
    )
  } else localStat <- NULL

  if (2 %in% type) {
    globalStat <- list(PIstats = PI.global.stats)
    k1 <- mean(PI.global.stats)
    k2 <- mean((PI.global.stats - k1)^2)
    k3 <- mean((PI.global.stats-k1)^3)
    globalStat$a <- k1 - 2*(k2^2)/k3
    globalStat$b <- k3/(4*k2)
    globalStat$k <- 8*(k2^3)/(k3^2)
    globalStat$control.limit.trans_chi2 <- qchisq(1 - fa.rate, globalStat$k)*globalStat$b + globalStat$a
    globalStat$control.limit.ecdf <- quantile(PI.global.stats,  1 - fa.rate)
    names(globalStat$control.limit.trans_chi2) <- names(globalStat$control.limit.ecdf)
  } else globalStat <- NULL

  out <- list(type = type, localStat = localStat, globalStat = globalStat)
  class(out) <- "climit"
  out
}
