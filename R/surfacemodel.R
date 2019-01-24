surfacemodel <- function(img, nb, trim.vars = TRUE, cp = 1e-5, xval = 5,
                         standardize = TRUE, subsample = 1, verbose = FALSE) {

  if (!is.matrix(img)) stop("img must be a matrix!")

  if (missing(nb))
    stop("Missing nb!")
  else {
    if (!(length(nb) %in% c(1,3) && is.vector(nb)))
      stop("nb must be either a scalar or a vector of length 3!")
    if (any(nb < 1)) stop("nb must be positive!")
  }

  if (verbose)  cat("Building the model... ")
  ptm <- proc.time()

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
      cat("Initial fit completed!\n")
      cat("  User elapsed time: ", (proc.time() - ptm)[1], "\n")
    }
    if (xval > 1)
      if (verbose)  {
        cat("  Initial cross-validated R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")
        cat("Refitting using only variables used in the first fit...")
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
  if (trim.vars) out$vars <- vars
  class(out) <- "surfacemodel"

  if (verbose)  {
    cat("Completed!\n")
    cat("  Total user elapsed time: ", (proc.time() - ptm)[1], "\n")
    if (xval > 1) cat("  Cross-validated R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")
  }
  rm(list = setdiff(ls(), 'out'))
  out
}
