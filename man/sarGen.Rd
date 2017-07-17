\name{sarGen}
\alias{sarGen}
\title{
Stochastic Autoregressive Image Generator}
\description{
Generates 2D stochastic AR(1) images with/without a local defect.
}
\usage{
sarGen(phi1 = .6, phi2 = .35, sigma = .01, m = 250, n = 250, border = 200)
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
  \item{border}{
the number of top rows/left columns to be cut off from the generated image. This helps reduce the effect of the starting condition.
}
}
\details{
The pixel \code{y(i,j)} of the 2D AR(1) process satisfies: \code{y(i,j) = phi1*y(i-1,j) + phi2*y(i,j-1) + e(i,j)}, where \code{e(i,j)} follows a zero-mean Gaussian distribution with standard deviation of \code{sigma}. The process is then rescaled to [0, 255] to produce a greyscale image.
}
\value{
The generated image in the matrix format.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\seealso{
\code{\link{imposeDefect}}
}
\examples{
## generate an image without defects
img <- sarGen(m = 100, n = 100, border = 50)
image(img,col=gray(c(0:32)/32))
}
