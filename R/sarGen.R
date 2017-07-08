sarGen <- function(phi1 = .6, phi2 = .35, sigma = .01, m = 250, n = 250,
                   defect = FALSE, loc = NULL, a = 7, b = 10, eps = .05,
                   phi1_new = 0, phi2_new = 0, sigma_new = sigma,
                   border = 200, greyscale = FALSE) {

  if (phi1 + phi2 > 1 || phi1 + phi2 <0) stop("phi1 + phi2 should be in [0, 1)!")

  newImg <- matrix(0,m+border,n+border)

  if (!defect) {
    for (i in 2:nrow(newImg)) {
      for (j in 2:ncol(newImg)) {
        newImg[i,j] <- phi1*newImg[i-1,j] + phi2*newImg[i,j-1] + rnorm(1,0,sigma)
      }
    }
  } else {
    if (is.null(loc)) loc <- c(sample((a+1):(m-a),1),sample((b+1):(n-b),1)) + border
    for (i in 2:nrow(newImg)) {
      for (j in 2:ncol(newImg)) {
        if ( (i-loc[1])^2/a^2 + (j-loc[2])^2/b^2 <= (1 + eps) ) # if the pixel is within the defective ellipsoid
          newImg[i,j] <- phi1_new*newImg[i-1,j] + phi2_new*newImg[i,j-1] + rnorm(1,0,sigma_new)
        else
          newImg[i,j] <- phi1*newImg[i-1,j] + phi2*newImg[i,j-1] + rnorm(1,0,sigma)
      }
    }
  }


  newImg <- newImg[(border+1):(m+border),(border+1):(n+border)]

  if (greyscale) {
    newImg <- round((newImg - min(newImg))/(max(newImg) - min(newImg))*255)
    newImg <- apply(newImg,1:2,as.integer)
  }

  if (defect) list(img = newImg,
                   defect.info = list(type = "smooth",
                                      defect.center = loc - border,
                                      defect.shape = c(a, b, eps),
                                      defect.params = c(phi1_new, phi2_new, sigma_new)
                   )
  )
  else img = newImg

}
