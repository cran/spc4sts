diagnoseLD <- function(ms, dth, plot.it = TRUE) {

  if (class(ms) != "monitoringStat")
    stop("ms must be an object returned by the monitoringStat function!")
  if (is.null(ms$sms))
    stop("ms does not contain information for diagnosing local defects. Need to set type = 1 when running monitoringStat().")

  bimg = ms$sms;
  bimg[which(ms$sms <= dth, arr.ind=TRUE)] <- 1
  bimg[which(ms$sms > dth, arr.ind=TRUE)] <- 0

  if (plot.it)
    image(1:ncol(bimg), 1:nrow(bimg), as.matrix(t(apply(bimg , 2, rev))),
          col = c(0,1), xlab="", ylab="")

  invisible(bimg)

}
