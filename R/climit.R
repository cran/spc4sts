climit <- function (imgs, fa.rate, model, type, stat = c("ad", "bp"),
          w, nD = 10, no_cores = 1, verbose = FALSE)
{
  if (verbose) {
    ptm <- proc.time()
    cat("Computing control limits... ")
  }
  if (!is.array(imgs))
    stop("img must be a 3-dimensional array.")
  else if (length(dim(imgs)) != 3)
    stop("img must be a 3-dimensional array.")
  if (missing(type))
    stop("A false alarm rate(s) must be provided.")
  if (class(model) != "surfacemodel")
    stop("Wrong input for the model argument! Needs a surfacemodel object.")
  if (missing(type) | length(type) > 2 | !all(type %in% 1:2))
    stop("Missing type of monitoring statistic.\n           It must be 1 (for local defect), 2 (for global change), or 1:2 (for both).")
  N <- dim(imgs)[3]
  if (1 %in% type) {
    if (missing(w))
      stop("Missing argument w. It must be an odd number >= 3.")
    else if (w < 3 || w%%2 < 1)
      stop("w must be an odd number >= 3.")
    stat <- match.arg(stat)
    M <- dim(imgs)[1] * dim(imgs)[2]
    nD <- nD * N
    nD.max = max(nD, round(dim(imgs)[1] * dim(imgs)[2] *
                             0.0016))
    tmp <- NULL
  }

  if (no_cores > 1) {
    cluster <- makeCluster(no_cores)
    clusterExport(cluster, "monitoringStat")
    clusterExport(cl=cluster,
                  varlist=c("model", "type", "stat", "w"),
                  envir=environment())
    mStat <- parApply(cluster, imgs, 3,
                      function(x) monitoringStat(x, model = model,
                                                 type = type, stat = stat, w = w))
    if (2 %in% type)
      PI.global.stats <- parSapply(cluster, mStat, function(x) x$globalStat)
    stopCluster(cluster)
    if (1 %in% type) {
      PI.local.stats <- vector("numeric", N)
      for (j in 1:N) {
        tmp <- sort(c(tmp, mStat[[j]]$sms), decreasing=T)[1:(nD.max+1)]
        PI.local.stats[j] <- mStat[[j]]$localStat
      }
    }
  } else {
    PI.local.stats <- PI.global.stats <- vector("numeric", N)
    for (j in 1:N) {
      mStat <- monitoringStat(img = imgs[, , j], model = model,
                              type = type, stat = stat, w = w)
      if (1 %in% type) {
        tmp <- sort(c(tmp, mStat$sms), decreasing = T)[1:(nD.max+1)]
        PI.local.stats[j] <- mStat$localStat
      }
      if (2 %in% type)
        PI.global.stats[j] <- mStat$globalStat
      gc(verbose = FALSE)
      if (verbose)
        cat(round(j/N * 100), "%-", sep = "")
    }
  }

  if (1 %in% type) {
    localStat <- list(nDmaxSms = tmp, PIstats = PI.local.stats,
                      diagnostic.threshold = (tmp[nD] + tmp[nD + 1])/2,
                      stat = stat, w = w, control.limit = quantile(PI.local.stats,
                                                                   1 - fa.rate))
  }
  else localStat <- NULL
  if (2 %in% type) {
    globalStat <- list(PIstats = PI.global.stats)
    k1 <- mean(PI.global.stats)
    k2 <- var(PI.global.stats)
    k3 <- mean((PI.global.stats - k1)^3)
    k <- 8 * (k2^3)/(k3^2)
    b <- k3/(4 * k2)
    a <- k1 - 2 * (k2^2)/k3
    if (b >= 0)
      globalStat$control.limit.trans_chi2 <- qchisq(1 - fa.rate, k) * b + a
    else globalStat$control.limit.trans_chi2 <- qchisq(fa.rate, k) * b + a
    globalStat$control.limit.ecdf <- quantile(PI.global.stats,
                                              1 - fa.rate)
    names(globalStat$control.limit.trans_chi2) <- names(globalStat$control.limit.ecdf)
  }
  else globalStat <- NULL
  out <- list(type = type, fa.rate = fa.rate, localStat = localStat,
              globalStat = globalStat)
  class(out) <- "climit"
  if (verbose) {
    cat("Completed!\n")
    cat("Total elapsed time: ", (proc.time() - ptm)[1], "\n")
  }
  out
}
