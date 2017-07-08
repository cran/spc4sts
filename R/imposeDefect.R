imposeDefect <- function(img, loc = NULL, a = 7, b = 10, eps=.05, phi1 = 0, phi2 = 0, sigma = .01) {

  if (is.null(loc)) {
    m <- nrow(img)
    n <- ncol(img)
    loc <- c(sample((a+1+4):(m-a-4),1),sample((b+1+4):(n-b-4),1))
  }

  gscale <- FALSE
  if (is.integer(img))  gscale <- TRUE
  defect <- sarGen(phi1, phi2, sigma, 2*a+1, 2*b+1, defect = FALSE, greyscale = gscale)

  for (i in (loc[1] - a):(loc[1] + a)) {
    for (j in (loc[2] - b):(loc[2] + b)) {
      if ( (i-loc[1])^2/a^2 + (j-loc[2])^2/b^2 <= (1+ eps) ) {# if the pixel is within the defective ellipsoid
        img[i,j] <- defect[i-(loc[1] - a)+1,j-(loc[2] - b)+1]
      }
    }
  }

  list(img = img,
       defect.info = list(type = "superimposed",
                          defect.center = loc,
                          defect.shape = c(a, b, eps),
                          defect.params = c(phi1, phi2, sigma)
       )
  )

}
