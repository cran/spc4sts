surfacemodel <- function(img, nb, control, y = FALSE){

  img <- as.matrix(img)
  img <- (img - mean(img))/sd(img)

  if (length(nb)!=3)
    if (length(nb)==1) nb <- rep(nb,3)
    else stop("Incorrect neighborhood size (must be either a scalar or a vector of length 3)!")

  cat("Constructing the neighborhood data...")
  dat <- dataPrep(img, nb)

  cat("Done!\n Fitting a regression tree...")
  if (missing(control)) control<- rpart.control(minsplit=10, cp=0.00001, xval=10)
  fit <- rpart(V1~.,dat,method = "anova",control = control,y = TRUE)
  cp.min.id <- which.min(fit$cptable[,"xerror"])
  fit <- prune(fit, cp = fit$cptable[cp.min.id,"CP"])
  cat("Done!\n")

  Fr <- exptailecdf(residuals(fit))
  if (!y) fit$y <- NULL

  list(fit = fit, R2cv = 1 - fit$cptable[cp.min.id,4], nb = nb, Fr = Fr)

}
