sarGen <- function(phi1 = .6, phi2 = .35, sigma = .01, m = 250, n = 250, border = 200) {

  if (phi1 + phi2 > 1 || phi1 + phi2 <0) stop("phi1 + phi2 should be in [0, 1)!")

  newImg <- matrix(0, m + border, n + border)

  for (i in 2:nrow(newImg)) {
    for (j in 2:ncol(newImg)) {
      newImg[i,j] <- phi1*newImg[i-1,j] + phi2*newImg[i,j-1] + rnorm(1,0,sigma)
    }
  }

  newImg <- newImg[(border+1):(m+border),(border+1):(n+border)]
  newImg <- round((newImg - min(newImg))/(max(newImg) - min(newImg))*255)
  newImg <- apply(newImg,1:2,as.integer)

  newImg

}
