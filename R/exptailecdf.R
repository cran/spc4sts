exptailecdf <- function(x,N=max(2,.002*length(x)),m=min(N,5)) {

  x <- sort(x)
  n <- length(x)
  Fx <- ecdf(x)

  ltail <- x[1:N]
  lambda.left <- -1/mean(ltail-max(ltail))
  joint.left <- x[m+1]

  rtail <- x[(n-N+1):n]
  lambda.right <- 1/mean(rtail-min(rtail))
  joint.right <- x[n-m+1]

  out <- list(ecdf = Fx,
       lambda = c(lambda.left,lambda.right),
       joint = c(joint.left,joint.right)
  )
  class(out) <- "exptailecdf"
  out
}
