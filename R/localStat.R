localStat <- function(img, model, stat = c("ad","bp"), w) {

  if (missing(stat)) stat = "bp"

  img <- as.matrix(img)
  img <- (img - mean(img))/sd(img)
  dat <- dataPrep(img, model$nb)
  res <- matrix(dat[,1] - as.numeric(predict(model$fit, dat)), nrow(img) - model$nb[1],
                ncol(img) - sum(model$nb[2:3]), byrow=TRUE);
  ms <- sms(res, stat, w, Fr = model$Fr) # SMS

  # pad 0's to make ms have the same size with img
  w2 <- (w - 1)%/%2
  m1 <- nrow(ms)
  m2 <- ncol(ms)
  s1 <- w2 + model$nb[1] + 1
  s2 <- w2 + model$nb[2] + 1
  e1 <- s1 + m1 - 1
  e2 <- s2 + m2 - 1
  ms2 <- matrix(0,e1+w2,e2+w2+model$nb[3])
  ms2[s1:e1,s2:e2] <- ms # SMS with the size of the original image

  list(sms = ms2, stat = stat, windowSize = w, monitoringStat = max(ms))
}
