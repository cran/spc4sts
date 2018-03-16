mbChange <- function(img, alpha = 1) {

  if (!is.matrix(img)) stop("img must be a matrix!")

  nr <- nrow(img)
  nc <- ncol(img)
  newImg <- img
  for (i in 1:nc)
    newImg[2:nr,i] <- (1 - alpha*(i-1)/nc)*newImg[2:nr,i] + alpha*(i-1)/nc*newImg[1:(nr-1),i]

  return(newImg)
}
