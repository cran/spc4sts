bp <- function(img, i1, i2, w, K = kerMat((w + 1) %/% 2))
{
  w3 <- (w - 1) %/% 2
  S <- vector(mode = "numeric", length = w^2)
  id <- 1
  for (j1 in (i1-w3):(i1+w3)) {
    for (j2 in (i2-w3):(i2+w3)) {
      S[id] <- spaCov(img,i1,i2,j1,j2,K)
      id <- id + 1
    }
  }

  sum(S^2)

}
