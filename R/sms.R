sms <- function(img, stat, ms.ws, Fr, gamma = (ms.ws + 1)%/%2) {

  if (ms.ws<=1) {
    cat("Moving window size less than 3. Return NULL! \n")
    invisible(NULL)
  }

  d1 <- nrow(img)
  d2 <- ncol(img)
  ms.ws2 <- ms.ws-1
  ms <- matrix(0,d1-ms.ws2,d2-ms.ws2)
  if (stat=="ad") {
    P <- pexptailecdf(Fr,img)
    for (i in 1:(d1-ms.ws2)) {
      for (j in 1:(d2-ms.ws2)) {
        ms[i,j] <- ad(img[i:(i+ms.ws2),j:(j+ms.ws2)],P[i:(i+ms.ws2),j:(j+ms.ws2)])
      }
    }
  } else if (stat=="bp") {
    if (gamma <= 0) { # limiting case
      e2 <- img^2
      center <- (ms.ws+1)/2
      for (i in 1:(d1-ms.ws2)) {
        for (j in 1:(d2-ms.ws2)) {
          x <- e2[i:(i+ms.ws2),j:(j+ms.ws2)]
          ms[i,j] <- x[center,center]*sum(x)
        }
      }
    } else {
      K <- kerMat(gamma)
      p2 <- gamma - 1
      ms.ws3 <- ms.ws2/2
      # loops for pixels without the Kernel boundary problem
      for (i in (1+p2):(d1-ms.ws2-p2)) {
        for (j in (1+p2):(d2-ms.ws2-p2)) {
          ms[i,j] <- bp2(img,i+ms.ws3,j+ms.ws3,ms.ws,K)
        }
      }

      # loops for (top/bottom) pixels with the Kernel boundary problem
      for ( i in c( 1:p2,(d1-ms.ws2-p2+1):(d1-ms.ws2) ) ) {
        for (j in 1:(d2-ms.ws2)) {
          ms[i,j] <- bp(img,i+ms.ws3,j+ms.ws3,ms.ws,K)
        }
      }

      # loops for (left/right-sided) pixels with the Kernel boundary problem
      for ( i in gamma:(d1-ms.ws2-p2) ) {
        for (j in c( 1:p2,(d2-ms.ws2-p2+1):(d2-ms.ws2) ) ) {
          ms[i,j] <- bp(img,i+ms.ws3,j+ms.ws3,ms.ws,K)
        }
      }

    }
  }
  invisible(ms)

}
