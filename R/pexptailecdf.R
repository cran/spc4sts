pexptailecdf <- function(Fx,y) {

  Py <- Fx$ecdf(y)

  id.l <- which(y <= Fx$joint[1])
  id.r <- which(y >= Fx$joint[2])

  coeff <- Fx$ecdf(c(Fx$joint[1],Fx$joint[2]))

  Py[id.l] <- max((1 - pexp(Fx$joint[1] - y[id.l], Fx$lambda[1]))*coeff[1], 1e-15)
  Py[id.r] <- min(pexp(y[id.r] - Fx$joint[2], Fx$lambda[2])*(1 - coeff[2]) + coeff[2], 1-1e-15)

  if (is.matrix(y)) Py <- matrix(Py,nrow(y),ncol(y));
  Py
}
