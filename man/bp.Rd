\name{bp}
\alias{bp}
\alias{bp2}
\title{
Box-Pierce-Type Statistic
}
\description{
Compute a Box-Pierce-type (BP) statistic for pixels in a given image. \code{bp2()} cannot be used for pixels with the boundary problem, but is more efficient than \code{bp()} for other pixels.
}
\usage{
bp(img, i1, i2, w, K = kerMat((w + 1)/2))
bp2(img, i1, i2, w , K = kerMat((w + 1)/2))
}
\arguments{
  \item{img}{
the given image
}
  \item{i1}{
the row index of the pixel to compute the BP statistic for.
}
  \item{i2}{
the column index of the pixel to compute the BP statistic for.
}
  \item{w}{
the dimension of the spatial (square) moving window. Must be an odd number >= 3.
}
  \item{K}{
the weighted matrix.
}
}
\value{
The BP statistic.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\note{
\code{bp()} is only used in \code{sms()} for pixels with the boundary problem. It is less efficient than \code{bp2()} for other pixels.
}
\section{Warning }{For pixels with the boundary problem, \code{bp()} must be used.}

\seealso{
\code{\link{kerMat}, \link{spaCov}, \link{sms}, \link{ad}}
}
\examples{
img <- matrix(rnorm(100),10,10)

## for pixels with the boundary problem, e.g., Pixel (5,1),
# running bp2(img,5,1,3) will produce an error; instead, use bp() in this case:
bp(img,5,1,3)

## for pixels without the boundary problem, e.g., Pixel (5,5),
# both can be used, but bp2() is more efficient than bp()
bp2(img,5,5,3)
bp(img,5,5,3)
}

