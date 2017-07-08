\name{imposeDefect}
\alias{imposeDefect}
\title{
Superimpose Local Defects
}
\description{
Superimposes a local defect (a 2D stochastic AR(1) image from \code{sarGen}) on a given image.
}
\usage{
imposeDefect(img, loc = NULL, a = 7, b = 10, eps = 0.05, phi1 = 0, phi2 = 0, sigma = 0.01)
}
\arguments{
  \item{img}{
the image to be superimposed a defect.
}
  \item{loc}{
the location of the defect in the generated image.
}
  \item{a}{
\code{2*a + 1} is the vertical axis length of the ellipsoidal defect.
}
  \item{b}{
\code{2*b + 1} is the vertical axis length of the ellipsoidal defect.
}
  \item{eps}{
controls the curvature of the ellipsoidal defect.
}
  \item{phi1}{
the parameter \code{phi1} of the defect.
}
  \item{phi2}{
the parameter \code{phi2} of the defect.
}
  \item{sigma}{
the parameter \code{sigma} of the defect.
}
}
\details{
The pixel \code{y(i,j)} of the defect satisfies: \code{y(i,j) = phi1*y(i-1,j) + phi2*y(i,j-1) + e(i,j)}, where \code{e(i,j)} follows a zero-mean Gaussian distribution with standard deviation of \code{sigma}.
}
\value{
If \code{defect = TRUE}, a list of the following:
\item{img }{the generated image in the matrix format.}
\item{defect.info }{the information of the defects.}
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\note{
The the defect is superimposed on the given image, and therefore, there is a non-smooth transition from the normal  to defective areas. For smooth transition, add the defect directly when generating the image with \code{sarGen}.
}
\seealso{
\code{\link{sarGen}}
}
\examples{
## generate an image without defects
img <- sarGen(m = 100, n = 100, border = 50)
image(img,col=gray(c(0:32)/32))

## superimpose a defect
img <- imposeDefect(img)
image(img$img,col=gray(c(0:32)/32))
}
