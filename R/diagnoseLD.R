diagnoseLD <- function(ls, dth, plot.it = TRUE) {

  bimg = ls$sms;
  bimg[which(ls$sms <= dth, arr.ind=TRUE)] <- 1
  bimg[which(ls$sms > dth, arr.ind=TRUE)] <- 0

  if (plot.it)
    image(1:ncol(bimg), 1:nrow(bimg), as.matrix(t(apply(bimg , 2, rev))),
          col = gray((0:32)/32), xlab="", ylab="")

  invisible(bimg)

}
