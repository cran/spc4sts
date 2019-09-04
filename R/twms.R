twms <- function(x, type=c('ewma', 'cusum'), lambda, mu0, K, x0=0) {

  y1 <- x0
  y2 <- matrix(x0,2,1)
  row.names(y2) <- c('upper', 'lower')
  type <- match.arg(type)
  switch(type,
         ewma = sapply(x, function(u) y1 <<- y1*(1 - lambda) + u*lambda),
         cusum = vapply(x,
                        function(u) y2 <<- c(max(0, u - mu0 - K + y2[1]),
                                               max(0, -u + mu0 - K + y2[2])),
                        c(upper = 0, lower = 0)
                        )
         )

}
