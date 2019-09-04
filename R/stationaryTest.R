stationaryTest <- function(img, nsamples = 100,...) {

  capture.output(out <- LS2Wstat::TOS2D(img, nsamples = nsamples, ...))
  out

}
