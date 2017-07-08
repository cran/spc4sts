\name{sarGen}
\alias{sarGen}
\title{
Stochastic Autoregressive Image Generator}
\description{
Generates 2D stochastic AR(1) images with/without a local defect.
}
\usage{
sarGen(phi1 = .6, phi2 = .35, sigma = .01, m = 250, n = 250,
       defect = FALSE, loc = NULL, a = 7, b = 10, eps = .05,
       phi1_new = 0, phi2_new = 0, sigma_new = sigma,
       border = 200, greyscale = FALSE)
}
\arguments{
  \item{phi1}{
the parameter \code{phi1}  of the process.
}
  \item{phi2}{
the parameter \code{phi2} of the process.
}
  \item{sigma}{
the parameter \code{sigma} of the process.
}
  \item{m}{
the number of rows of the generated image.
}
  \item{n}{
the number of columns of the generated image.
}
  \item{defect}{
if set to \code{TRUE}, (smoothly) adds a defect (an ellipsoidal-shaped 2D AR(1) image) to the generated one.
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
  \item{phi1_new}{
the parameter \code{phi1} of the defect.
}
  \item{phi2_new}{
the parameter \code{phi2} of the defect.
}
  \item{sigma_new}{
the parameter \code{sigma} of the defect.
}
  \item{border}{
the number of top rows/left columns to be cut off from the generated image. This helps reduce the effect of the starting condition.
}
  \item{greyscale}{
whether to return greyscale (0-255) or continuous pixel intensities.
}
}
\details{
The pixel \code{y(i,j)} of the generated image satisfies: \code{y(i,j) = phi1*y(i-1,j) + phi2*y(i,j-1) + e(i,j)}, where \code{e(i,j)} follows a zero-mean Gaussian distribution with standard deviation of \code{sigma}.
}
\value{
If \code{defect = TRUE}, a list of the following:
\item{img }{the generated image in the matrix format.}
\item{defect.info }{the information of the defects.}
Otherwise, the generated image in the matrix format.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\note{
The the defect is generated with the boundary from the generated image. Hence, there is a smooth transition from the normal  to defective areas. For non-smooth transition, use \code{imposeDefect}.
}
\seealso{
\code{\link{imposeDefect}}
}
\examples{
## generate an image without defects
img <- sarGen(m = 100, n = 100, border = 50)
image(img,col=gray(c(0:32)/32))

## generate an image with a defect
img <- sarGen(m = 100, n = 100, border = 50, defect = TRUE)
image(img$img,col=gray(c(0:32)/32))
}
