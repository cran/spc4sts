surfacemodel <- function(img, nb, trim.vars = TRUE, control, y = FALSE){

  if (!is.matrix(img)) stop("img must be a matrix!")

  if (missing(nb))
    stop("Missing nb!")
  else {
    if (!(length(nb) %in% c(1,3) && is.vector(nb)))
      stop("nb must be either a scalar or a vector of length 3!")
    if (any(nb < 1)) stop("nb must be positive!")
  }

  cat("Building the model... ")
  ptm <- proc.time()

  if (length(nb)==1) nb <- rep(nb,3)

  img <- (img - mean(img))/sd(img)

  dat <- dataPrep(img, nb)

  if (missing(control))
    control<- rpart.control(minsplit=10, cp=0.00001, xval=10, maxsurrogate = 0)
  fit <- rpart(V1~.,dat,method = "anova",control = control,y = TRUE)
  cp.min.id <- which.min(fit$cptable[,"xerror"])
  fit <- prune(fit, cp = fit$cptable[cp.min.id,"CP"])

  if (trim.vars) {
    cat("Initial fit completed!\n")
    cat("  User elapsed time: ", (proc.time() - ptm)[1], "\n")
    cat("  Initial cross-validated R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")
    cat("Refitting using only variables used in the first fit...")
    vars <- names(fit$variable.importance)
    dat <- dat[,c("V1",vars)]
    fit <- rpart(V1~.,dat,method = "anova",control = control,y = TRUE)
    cp.min.id <- which.min(fit$cptable[,"xerror"])
    fit <- prune(fit, cp = fit$cptable[cp.min.id,"CP"])
  }

  Fr <- exptailecdf(residuals(fit))
  if (!y) fit$y <- NULL

  out <- list(fit = fit, trim.vars = trim.vars, R2cv = 1 - fit$cptable[cp.min.id,4], nb = nb, Fr = Fr)
  class(out) <- "surfacemodel"

  cat("Completed!\n")
  cat("  Total user elapsed time: ", (proc.time() - ptm)[1], "\n")
  cat("  Cross-validated R-squared: ", 1 - fit$cptable[cp.min.id,4], "\n")

  out
}
