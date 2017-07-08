\name{diagnoseLD}
\alias{diagnoseLD}
\title{
Diagnose Local Defects on Stochastic Textured Surfaces
}
\description{
Produces a binary diagnostic image of a given stochastic textured surface image based on its spatial moving statistics.
}
\usage{
diagnoseLD(ls, dth, plot.it = TRUE)
}
\arguments{
  \item{ls}{
the output from \code{localStat()}
}
  \item{dth}{
the diagnostic threshold
}
  \item{plot.it}{
plots the binary diagnositc image if set to \code{TRUE}
}
}
\value{
The binary diagnostic image in matrix format.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\seealso{
\code{\link{localStat}, \link{climit}}
}
\examples{
## build the in-control model
img <- sarGen(m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img,1)

## diagnose a Phase II image
img2 <- sarGen(m = 100, n = 100, border = 50)
img2 <- imposeDefect(img2)
image(as.matrix(t(apply(img2$img , 2, rev))),col=gray(c(0:32)/32))

ls <- localStat(img2$img, model, "bp", 5)

bimg <- diagnoseLD(ls, dth = 1.4) # use climit() to find dth
}
