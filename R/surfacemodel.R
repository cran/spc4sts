surfacemodel <- function(img, nb, trim.vars = TRUE, cp = 1e-5,
                         xval = 5, standardize = TRUE, subsample = 1,
                         verbose = FALSE, keep.residuals = FALSE,
                         stationary.test = FALSE, conf.level = .95, nsamples = 20) {

  if (!is.matrix(img)) stop("img must be a matrix!")

  if (missing(nb))
    stop("Missing nb!")
  else {
    if (!(length(nb) %in% c(1,3) && is.vector(nb)))
      stop("nb must be either a scalar or a vector of length 3!")
    if (any(nb < 1)) stop("nb must be positive!")
  }
  ptm <- proc.time()
  if (stationary.test) {
    if (verbose)
      cat("Testing for stationary assumption... ")
    pvalue <- stationaryTest(img, nsamples = nsamples, verbose = verbose)$p.value
    if (verbose)  {
      cat("Completed!\n")
      cat("  Elapsed time: ", (proc.time() - ptm)[3], "\n")
    }
    if (pvalue <= 1 - conf.level) {
      cat('  The given image is not stationary at confidence level:', conf.level, '\n')
      cat('  The procedure is stopped. Return the p-value of the stationary test:', pvalue, '\n')
      return(pvalue)
    } else if (verbose) {
      cat('  The given image is stationary at confidence level:', conf.level, '\n')
      cat('  The p-value is', pvalue, '\n')
    }
  }

  if (verbose)  {
    cat("Building the model... ")
  ptm2 <- proc.time()
  }

  if (length(nb)==1) nb <- rep(nb,3)

  if (standardize)  img <- (img - mean(img))/sd(img)

  dat <- dataPrep(img = img, nb = nb, subsample = subsample)

  control <- rpart.control(minsplit = 10, cp = cp, xval = xval, maxsurrogate = 0)
  fit <- rpart(V1~., dat, method = "anova", control = control, y = TRUE)

  xval <- control$xval
  if (xval > 1) {
    cp.min.id <- which.min(fit$cptable[,"xerror"])
    fit <- prune(fit, cp = fit$cptable[cp.min.id,"CP"])
  }

  if (trim.vars) {
    if (verbose)  {
      cat("First fit completed!\n")
      cat("  Elapsed time: ", (proc.time() - ptm2)[3], "\n")
    }
    if (xval > 1)
      if (verbose)  {
        cat("  Cross-validation R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")
        cat("Refitting using only important variables in the first fit... ")
      }
    vars <- names(fit$variable.importance)
    fit <- rpart(V1~.,dat[,c("V1",vars)],method = "anova",control = control, y = TRUE)
    if (xval > 1) {
      cp.min.id <- which.min(fit$cptable[,"xerror"])
      fit <- prune(fit, cp = fit$cptable[cp.min.id,"CP"])
    }
  }

  r <- residuals(fit)
  fit$y <- NULL

  out <- list(fit = fit,
              trim.vars = trim.vars,
              nb = nb,
              Fr = exptailecdf(r),
              MSE = mean(r^2),
              standardize = standardize
  )

  if (xval > 1) {
    out$R2cv <- 1 - fit$cptable[cp.min.id, 4]
    out$complexity <- fit$cptable[cp.min.id, 1]
  } else
    out$complexity <- cp
  if (trim.vars)
    out$vars <- vars
  if (stationary.test) {
    out$p.value <- pvalue
    out$nsamples <- nsamples
  }
  if (keep.residuals)
    out$residuals <- r
  if (verbose)  {
    cat("Completed!\n")
    cat("  Total elapsed time: ", (proc.time() - ptm)[3], "\n")
    if (xval > 1) cat("  Cross-validation R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")
  }

  class(out) <- "surfacemodel"
  rm(list = setdiff(ls(), "out"))
  out
}
