dataPrep <- function(img, nb, vars = NULL) {

  if (!is.matrix(img)) stop("img must be a matrix!")
  if (missing(nb))
    stop("Missing nb!")
  else {
    if (!(length(nb) %in% c(1,3) && is.vector(nb)))
      stop("nb must be either a scalar or a vector of length 3!")
    if (any(nb < 1)) stop("nb must be positive!")
  }
  if (length(nb)==1) nb <- rep(nb,3)

  h <- nb[1]; w1 <- nb[2]; w2 <- nb[3];

  n2 <- nrow(img) - h;  m2 <- ncol(img) - w1 - w2;

  if (is.null(vars)) {
    n <- (w1+w2+1)*h+w1+1 	# find number of neighbors
    data <- matrix(0,n2*m2,n);    	  # stack rows in window of each pixel
    k <- 1
    for (i in 1:n2) {
      for (j in 1:m2 ) {
        data[k,] <- c(img[i+h,j+w1],
                      img[i:(i+h),j:(j+w1-1)],
                      img[i:(i+h-1),(j+w1):(j+w1+w2)])
        k <- k + 1
      }
    }
    data <- as.data.frame(data)
  } else {
    vars <- c("V1",vars);
    vars.id<-unique(na.omit(as.numeric(unlist(strsplit(unlist(vars), "[^0-9]+")))))
    n <- length(vars.id);
    data <- matrix(0,n2*m2,n);
    k <- 1
    for (i in 1:n2) {
      for (j in 1:m2 ) {
        data[k,] <- matrix(c(img[i+h,j+w1],
                             img[i:(i+h),j:(j+w1-1)],
                             img[i:(i+h-1),(j+w1):(j+w1+w2)])[vars.id],1,n)
        k <- k + 1
      }
    }
    data <- as.data.frame(data)
    colnames(data) <- vars
  }
  data
}
