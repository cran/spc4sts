plotcc <- function(statsII, CL, statsI=NULL) {

  n2 <- length(statsII)
  if (is.null(statsI))
    n1 <- 0
  else
    n1 <- length(statsI)

  pch <- rep(1, n1+n2)
  stat <- c(statsI,statsII)
  for (j in 1:(n1+n2))
    if (stat[j] > CL)
      pch[j] <- 16

  plot(stat, type='b', pch=pch, xaxt='n', xlab='Image index', ylab='Monitoring statistic')
  axis(1, at=1:(n1+n2), labels=c(rep(NA,n1),1:n2))
  abline(h=CL, lty=2)
  abline(v=n1+.5, lty=1)

}
