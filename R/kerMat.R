kerMat <- function(p) {

  A <- matrix(0,p,p)
  p2 <- p^2
  for (i in 1:p) {
    for (j in 1:i) {
      norm2 <- (i-p)^2+(j-p)^2
      if (norm2 < p2) {
        A[i,j] <- A[j,i] <- .75*( 1 - norm2/p2 ) # Epanechnikov quadratic kernel
      }
    }
  }
  B <- rbind(A,apply(A,2,rev)[-1,])

  cbind(B,t(apply(B,1,rev))[,-1])

}
