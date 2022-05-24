sms <- function (img, stat = c("ad", "bp"), w, Fr, gamma = (w + 1)/2)
{
  if (!is.matrix(img))
    stop("img must be a matrix.")
  stat <- match.arg(stat)
  if (w < 3 || w%%2 < 1)
    stop("w < 3 or not an odd number. Return NULL! \n")
  d1 <- nrow(img)
  d2 <- ncol(img)
  w2 <- w - 1
  ms <- matrix(0, d1 - w2, d2 - w2)
  if (stat == "ad") {
    if (!inherits(Fr, "exptailecdf"))
      stop("Fr must be an object returned by the exptailecdf function!")
    P <- pexptailecdf(Fr, img)
    for (i in 1:(d1 - w2)) {
      for (j in 1:(d2 - w2)) {
        ms[i, j] <- ad(img[i:(i + w2), j:(j + w2)],
                       P[i:(i + w2), j:(j + w2)])
      }
    }
  }
  else if (stat == "bp") {
    if (gamma < 0)
      stop("gamma must be a positive integer!")
    if (gamma == 0) {
      e2 <- img^2
      center <- (w + 1)/2
      for (i in 1:(d1 - w2)) {
        for (j in 1:(d2 - w2)) {
          x <- e2[i:(i + w2), j:(j + w2)]
          ms[i, j] <- x[center, center] * sum(x)
        }
      }
    }
    else {
      K <- kerMat(gamma)
      p2 <- gamma - 1
      w3 <- w2/2
      for (i in (1 + p2):(d1 - w2 - p2)) {
        for (j in (1 + p2):(d2 - w2 - p2)) {
          ms[i, j] <- bp2(img, i + w3, j + w3, w, K)
        }
      }
      for (i in c(1:p2, (d1 - w2 - p2 + 1):(d1 - w2))) {
        for (j in 1:(d2 - w2)) {
          ms[i, j] <- bp(img, i + w3, j + w3, w, K)
        }
      }
      for (i in gamma:(d1 - w2 - p2)) {
        for (j in c(1:p2, (d2 - w2 - p2 + 1):(d2 - w2))) {
          ms[i, j] <- bp(img, i + w3, j + w3, w, K)
        }
      }
    }
  }
  invisible(ms)
}
