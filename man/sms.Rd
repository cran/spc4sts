\name{sms}
\alias{sms}
\title{
Spatial Moving Statistic
}
\description{
Computes the spatial moving statistics (SMS) for pixels in a given image.
}
\usage{
sms(img, stat, ms.ws, Fr, gamma = (ms.ws + 1)\%/\%2)
}
\arguments{
  \item{img}{
the image to compute the SMS for.
}
  \item{stat}{
the statistic used in the SMS. Can be either "ad" or "bp".
}
  \item{ms.ws}{
the moving window size. Must be an odd number.
}
  \item{Fr}{
the reference ecdf with exponential tail approximation (see \code{\link{exptailecdf}}). Only used when \code{stat = "ad"}.
}
  \item{gamma}{
the bandwidth parameter for \code{\link{kerMat}}. Only used when \code{stat = "bp"}. The default value is recommended.
}
}
\value{
A matrix containing the SMS values computed for the pixels in \code{img}.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\seealso{
\code{\link{ad}, \link{bp}, \link{localStat}}
}
\examples{
img <- matrix(rnorm(100),10,10)
ms.ad <- sms(img, "ad", 3, exptailecdf(rnorm(1000)))
ms.bp <- sms(img, "bp", 3)
}
