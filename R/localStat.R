localStat <- function(img, model, cl = NULL, stat = c("ad", "bp"), w) {

  if (!is.matrix(img)) stop("img must be a matrix.")
  if (class(model) != "surfacemodel") stop("Wrong input for the model argument! Needs a surfacemodel object.")

  if (is.null(cl)) {
    if (missing(w)) stop("Missing argument w. It must be an odd number >= 3.")
    else if (w < 3 || w %% 2 < 1) stop("w must be an odd number >= 3.")
    stat <- match.arg(stat)
  } else {
    if (class(cl) != "climit")
      stop("cl must be an object returned by the climit or climit2 functions!")
    stat <- cl$stat
    w <- cl$w
  }

  img <- (img - mean(img))/sd(img)

  if (model$trim.vars) {
    vars <- names(model$fit$variable.importance)
    dat <- dataPrep(img, model$nb, vars)
  } else
    dat <- dataPrep(img, model$nb)

  res <- matrix(dat[,1] - as.numeric(predict(model$fit, dat)), nrow(img) - model$nb[1],
                ncol(img) - sum(model$nb[2:3]), byrow=TRUE);

  ms <- sms(res, stat, w, Fr = model$Fr) # SMS

  # pad 0's to make ms have the same size with img
  w2 <- (w - 1)%/%2
  m1 <- nrow(ms)
  m2 <- ncol(ms)
  s1 <- w2 + model$nb[1] + 1
  s2 <- w2 + model$nb[2] + 1
  e1 <- s1 + m1 - 1
  e2 <- s2 + m2 - 1
  ms2 <- matrix(0,e1+w2,e2+w2+model$nb[3])
  ms2[s1:e1,s2:e2] <- ms # SMS with the size of the original image

  if (!is.null(cl)) {
    for (ucl in cl$control.limit) {
      cat("Monitoring statistic = ", max(ms), "\n")
      cat("At the control limit: ", ucl)
      if (max(ms) > ucl)        cat("  ->  Out-of-control!\n")
      else        cat("  ->  In-control!\n")
    }
  }

  out <- list(sms = ms2, stat = stat, windowSize = w, monitoringStat = max(ms))
  class(out) <- "localStat"
  out
}
