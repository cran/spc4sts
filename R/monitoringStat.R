monitoringStat <- function(img, model, type, stat = c("ad", "bp"), w, cl = NULL, verbose = FALSE) {

  if (!is.matrix(img))
    stop("img must be a matrix.")
  if (!inherits(model, "surfacemodel"))
    stop("Wrong input for the model argument! Needs a surfacemodel object.")

  if (is.null(cl)) {
    if (missing(type) | length(type) > 2 | !all(type %in% 1:2))
      stop("Missing type of monitoring statistic.
           It must be 1 (for local defect), 2 (for global change), or 1:2 (for both).")

    if (1 %in% type) {
      if (missing(w)) stop("Missing argument w. It must be an odd number >= 3.")
      else if (w < 3 || w %% 2 < 1) stop("w must be an odd number >= 3.")
      stat <- match.arg(stat)
    }

    if (2 %in% type) {
      control <- model$fit$control
      control$xval <- 0
      control$cp <- model$complexity
    }
  } else {
    if (!inherits(cl, "climit"))
      stop("cl must be an object returned by the climit or climit2 functions!")
    type <- cl$type

    if (1 %in% type) {
      stat <- cl$localStat$stat
      w <- cl$localStat$w
    }

    if (2 %in% type) {
      control <- model$fit$control
      control$xval <- 0
      control$cp <- model$complexity
    }
  }

  if (verbose) {
    ptm <- proc.time()
    cat('Preparing Data... ')
  }
  if (model$standardize) img <- (img - mean(img))/sd(img)
  dat <- dataPrep(img, model$nb)

  r0j <- dat[,1] - predict(model$fit, dat)
  if (verbose) {
    ptm1 <- proc.time()
    cat('Completed!\n  Elapsed time:', (ptm1 - ptm)[3], '\n')
  }
  ## local defect
  if (1 %in% type) {
    if (verbose)
      cat('Computing local defect monitoring statistic... ')
    res <- matrix(r0j, nrow(img) - model$nb[1],
                  ncol(img) - sum(model$nb[2:3]), byrow=TRUE)

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

    lStat <- max(ms)
    if (verbose) {
      ptm2 <- proc.time()
      cat('Completed!\n  Elapsed time:', (ptm2 - ptm1)[3], '\n')
      cat("  Statistic:", lStat, "\n")
      if (!is.null(cl)) {
        i <- 1
        for (ucl in cl$localStat$control.limit) {
          cat("  At the", names(cl$localStat$control.limit)[i],"control limit:", ucl)
          i <- i  + 1
          if (lStat > ucl)        cat(" -> Out-of-control!\n")
          else        cat(" -> In-control!\n")
        }
      }
    }

  }

  ## global change
  if (2 %in% type) {
    if (verbose)
      cat('Computing global change monitoring statistic... ')

    fit <- rpart(V1~., dat, method = "anova", control = control, y = TRUE)
    gStat <- log(model$MSE/mean(residuals(fit)^2)) + mean(r0j^2)/model$MSE - 1
    if (verbose) {
      ptm3 <- proc.time()
      cat('Completed!\n  Elapsed time:', (ptm3 - ptm2)[3], '\n')
      cat("  Statistic:", gStat, "\n")
      if (!is.null(cl)) {
        i <- 1
        if (!is.na(cl$globalStat$control.limit.trans_chi2)) {
          for (ucl in cl$globalStat$control.limit.trans_chi2) {
            cat("  At the", names(cl$globalStat$control.limit.trans_chi2)[i],
                "(transformed chi-squared) control limit: ", ucl)
            i <- i + 1
            if (gStat > ucl)        cat(" -> Out-of-control!\n")
            else        cat(" -> In-control!\n")
          }
        }
        i <- 1
        for (ucl in cl$globalStat$control.limit.ecdf) {
          cat("  At the", names(cl$globalStat$control.limit.trans_chi2)[i],
              "(ecdf) control limit: ", ucl)
          i <- i + 1
          if (gStat > ucl)        cat(" -> Out-of-control!\n")
          else        cat(" -> In-control!\n")
        }
      }
    }
  }

  out <- list()
  if (1 %in% type) {
    out$sms = ms2
    out$stat = stat
    out$windowSize = w
    out$localStat = lStat
  }

  if (2 %in% type) out$globalStat <- gStat

  class(out) <- "monitoringStat"
  rm(list = setdiff(ls(), "out"))
  out
}
