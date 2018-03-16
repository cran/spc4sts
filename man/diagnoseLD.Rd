\name{diagnoseLD}
\alias{diagnoseLD}
\title{
Diagnose Local Defects on Stochastic Textured Surfaces
}
\description{
Produces a binary diagnostic image of a given stochastic textured surface image based on its spatial moving statistics.
}
\usage{
diagnoseLD(ms, dth, plot.it = TRUE)
}
\arguments{
  \item{ms}{
the object return by \code{monitoringStat()}
}
  \item{dth}{
the diagnostic threshold
}
  \item{plot.it}{
plots the binary diagnositc image if set to \code{TRUE}
}
}
\value{
The binary diagnostic image in the matrix format.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{monitoringStat}, \link{climit}}
}
\examples{
## build the in-control model
img <- sarGen(phi1 = .6, phi2 = .35, m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img,1)

## diagnose a Phase II image
img2 <- sarGen(phi1 = .6, phi2 = .35, m = 100, n = 100, border = 50)
img2 <- imposeDefect(img2)
ms <- monitoringStat(img2$img, model, type = 1, stat = "ad", w = 5)
bimg <- diagnoseLD(ms, dth = 9, plot.it = FALSE) # use climit() to find dth
par(mfcol = c(1, 2))
par(mar = c(2, 0.5, 1, 0.5))
image(xaxt = 'n', yaxt = 'n', as.matrix(t(apply(img2$img , 2, rev))),
      col = gray((0:32)/32), xlab = '', ylab = '', asp = 1, bty = 'n')
image(xaxt = 'n', yaxt = 'n', as.matrix(t(apply(bimg , 2, rev))),
      col = gray(c(1, .5)), xlab = '', ylab = '', asp = 1, bty = 'n')
}
