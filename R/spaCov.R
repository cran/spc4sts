spaCov <- function(img,i1,i2,j1,j2,K) {

  m <- nrow(img)
  n <- ncol(img)
  p2 <- (nrow(K) - 1) %/% 2
  j1_p2 <- j1 - p2
  i1_p2 <- i1 - p2
  if (j1_p2>0 && i1_p2>0 ) {
    a1 <- 1     # starting row index for K
    a3 <- j1_p2 # starting row index for J
    a5 <- i1_p2 # starting row index for I
  } else {
    a1 <- -min(j1_p2,i1_p2) + 2;
    a3 <- j1_p2 + a1 - 1
    a5 <- i1_p2 + a1 - 1
  }

  j2_p2 <- j2 - p2
  i2_p2 <- i2 - p2
  if (j2-p2>0 && i2_p2>0) {
    b1 <- 1    # starting col index for K
    b3 <- j2_p2 # starting col index for J
    b5 <- i2_p2 # starting col index for I
  } else {
    b1 <- -min(j2_p2,i2_p2) + 2;
    b3 <- j2_p2 + b1 - 1
    b5 <- i2_p2 + b1 - 1
  }

  j1plusp2 <- j1 + p2
  i1plusp2 <- i1 + p2
  if (i1plusp2<=m && j1plusp2<=m) {
    a2 <- nrow(K) # ending row index for K
  } else {
    a2 <- nrow(K) - ( max(i1plusp2,j1plusp2) - m )
  }
  a4 <- a3 + (a2-a1) # ending row index for J
  a6 <- a5 + (a2-a1) # ending row index for I

  j2plusp2 <- j2 + p2
  i2plusp2 <- i2 + p2
  if (i2plusp2<=n && j2plusp2<=n) {
    b2 <- ncol(K)   # ending col index for K
  } else {
    b2 <- ncol(K) - ( max(i2plusp2,j2plusp2) - n )
  }
  b4 <- b3 + (b2-b1) # ending col index for J
  b6 <- b5 + (b2-b1) # ending col index for I

  K2 <- K[a1:a2,b1:b2]

  sum(K2*img[a5:a6,b5:b6]*img[a3:a4,b3:b4])/sum(K2)

}

