bp2 <- function (img, i1, i2, w, K)
{
  w3 <- (w - 1)/2
  p2 <- (nrow(K) - 1)/2
  S <- vector(mode = "numeric", length = w^2)
  id <- 1
  for (j1 in (i1 - w3):(i1 + w3)) {
    for (j2 in (i2 - w3):(i2 + w3)) {
      S[id] <- sum(K*
                   img[(i1 - p2):(i1 + p2), (i2 - p2):(i2 + p2)]*
                   img[(j1 - p2):(j1 + p2), (j2 - p2):(j2 + p2)]
                   )
      id <- id + 1
    }
  }
  S <- S/sum(K)
  sum(S^2)
}
