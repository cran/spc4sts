disMat <- function(imgs, nb, cp=1e-3, subsample = c(1, .5),
                   standardize = TRUE, keep.fits = FALSE, verbose=FALSE)
{
  if (!is.array(imgs)) stop("img must be a 3-dimensional array.")
  else if (length(dim(imgs)) != 3) stop("img must be a 3-dimensional array.")

  if (missing(nb))
    stop("Missing nb!")
  else {
    if (!(length(nb) %in% c(1,3) && is.vector(nb)))
      stop("nb must be either a scalar or a vector of length 3!")
    if (any(nb < 1)) stop("nb must be positive!")
  }

  if (any(subsample <= 0) || any(subsample > 1))
    stop("subsample elements must be in (0,1]!")
  else if (subsample[2] > subsample[1]) {
    subsample[2] <- subsample[1]
    cat("NOTE: subsample[2] > subsample[1]. Set subsample[2] <- subsample[1]!\n")
  }

  if (length(nb)==1) nb <- rep(nb,3)

  ## Standardize images if requested
  N <- dim(imgs)[3]
  if (standardize) {
    if (verbose) {
      cat("Stardardizing images... ")
      ptm <- proc.time()
    }
    for (i in 1:N) {
      imgs[,,i] <- (imgs[,,i] - mean(imgs[,,i]))/sd(imgs[,,i])
    }
    if (verbose)
      cat("Completed! User elapsed time: ", (proc.time() - ptm)[1], "\n")
  }
  ## Model fitting
  if (verbose) {
    cat("Building the models...")
    ptm <- proc.time()
  }
  fits <- list()
  for (j in 1:N) {
        fits[[j]] <- surfacemodel(imgs[,,j], nb, trim.vars = TRUE, cp = cp, xval = 0,
                              standardize = FALSE, subsample = subsample[1])
        if (verbose) cat(round(j/N*100), '%-', sep='')
  }
  if (verbose) {
    cat("Model fitting completed!\n")
    cat("  User elapsed time: ", (proc.time() - ptm)[1], "\n")
    cat("Computing dissimilarity matrices... ")
    ptm <- proc.time()
  }

  ## Computing dissimilarity matrices
  D3 <- D4 <- matrix(0,N,N)
  s <- 0
  if (subsample[1] <= subsample[2]) {
    for (i in 1:(N-1)) { # loop of 1st index
      dat_i <- dataPrep(img = imgs[,,i], nb = nb, subsample = subsample[2])
      yhat_ii <- fits[[i]]$fit$frame$yval[fits[[i]]$fit$where]
      for (j in (i+1):N) { # loop of 2nd index
        dat_j <- dataPrep(img = imgs[,,j], nb = nb, vars = fits[[i]]$vars, subsample = subsample[2])
        yhat_ij <- predict(fits[[i]]$fit, dat_j)
        MSEij <- mean((dat_j[,1] - yhat_ij)^2)
        yhat_ji <- predict(fits[[j]]$fit, dat_i)
        MSEji <- mean((dat_i[,1] - yhat_ji)^2)
        delta_i <- mean((yhat_ii - yhat_ji)^2)
        yhat_jj <- fits[[j]]$fit$frame$yval[fits[[j]]$fit$where]
        delta_j <- mean((yhat_jj - yhat_ij)^2)
        D3[i,j] <- -2 + (fits[[i]]$MSE+delta_i)/fits[[j]]$MSE + (fits[[j]]$MSE + delta_j)/fits[[i]]$MSE
        D4[i,j] <- delta_i + delta_j
      }
      if (verbose) {
        s <- s + N - i
        cat(round(s/N/(N-1)*50), '%-', sep='')
      }
    }
  } else {
    for (i in 1:(N-1)) { # loop of 1st index
      dat_i <- dataPrep(img = imgs[,,i], nb = nb, subsample = subsample[2])
      yhat_ii <- predict(fits[[i]]$fit, dat_i)
      for (j in (i+1):N) { # loop of 2nd index
        dat_j <- dataPrep(img = imgs[,,j], nb = nb,
                          vars = unique(c(fits[[i]]$vars, fits[[j]]$vars)), subsample = subsample[2])
        yhat_ij <- predict(fits[[i]]$fit, dat_j)
        MSEij <- mean((dat_j[,1] - yhat_ij)^2)
        yhat_ji <- predict(fits[[j]]$fit, dat_i)
        MSEji <- mean((dat_i[,1] - yhat_ji)^2)
        yhat_jj <- predict(fits[[j]]$fit, dat_j)
        delta_i <- mean((yhat_ii - yhat_ji)^2)
        delta_j <- mean((yhat_jj - yhat_ij)^2)
        D3[i,j] <- -2 + (fits[[i]]$MSE+delta_i)/fits[[j]]$MSE + (fits[[j]]$MSE + delta_j)/fits[[i]]$MSE
        D4[i,j] <- delta_i + delta_j
      }
      if (verbose) {
        s <- s + N - i
        cat(round(s/N/(N-1)*50), '%-', sep='')
      }
    }
  }
  D3 <- sqrt(D3/2)
  D4 <- sqrt(D4)
  out <- list(KL = D3 + t(D3), AKL = D4 + t(D4))

  if (verbose) {
    cat("Dissimilarity matrix computation completed!\n")
    cat("  User elapsed time: ", (proc.time() - ptm)[1], "\n")
  }

  if (keep.fits) save(fits, file=paste(getwd(),"/fits.Rdata",sep=""))
  rm(list = setdiff(ls(), 'out'))
  out
}
