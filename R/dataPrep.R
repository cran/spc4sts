dataPrep <- function(img, nb, vars = NULL, subsample = 1) {

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

  if (subsample <= 0 || subsample > 1)
    stop("subsample must be in (0,1]!")
  n2 <- nrow(img) - h # images row to be considered
  m2 <- ncol(img) - w1 - w2 # images column to be considered
  I <- as.integer(seq.int(1, n2, length.out=ceiling(subsample*n2)))
  J <- as.integer(seq.int(1, m2, length.out=ceiling(subsample*m2)))
  M <- length(I)*length(J)
  if (is.null(vars)) {
    n <- (w1+w2+1)*h + w1 + 1 	# find number of neighbors
    data <- matrix(0, M, n);    	  # stack rows in window of each pixel
    k <- 1
    for (i in I) {
      for (j in J) {
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
    data <- matrix(0,M,n);
    k <- 1
    for (i in I) {
      for (j in J ) {
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
