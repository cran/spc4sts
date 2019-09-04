\name{stationaryTest}
\alias{stationaryTest}
\title{
Stationary Test for Images}
\description{
This function is a wrapper for the \code{TOS2D{LS2Wstat}} function that tests if a given image is stationary or not.
}
\usage{
stationaryTest(img, nsamples = 100, ...)
}
\arguments{
  \item{img}{the given image to be tested.}
  \item{nsamples}{the number of bootstrap samples in the stationary test.}
  \item{\dots}{other arguments of the \code{\link{TOS2D}} function.}
}
\details{
See \code{\link{TOS2D}}.
}
\value{
See \code{\link{TOS2D}}.
}
