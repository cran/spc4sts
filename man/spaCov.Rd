\name{spaCov}
\alias{spaCov}
\title{
Spatial Weighted Covariance
}
\description{
Computes the spatial weighted covariance of a specified pair of pixels in a given image.
}
\usage{
spaCov(img, i1, i2, j1, j2, K)
}
\arguments{
  \item{img}{
the given image
}
  \item{i1}{
row index of the first pixel in the pair.
}
  \item{i2}{
column index of the first pixel in the pair.
}
  \item{j1}{
row index of the second pixel in the pair.
}
  \item{j2}{
column index of the second pixel in the pair.
}
  \item{K}{
the weighted matrix.
}
}
\value{
The spatial weighted covariance of a specified pair of pixels in a given image.
}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui
}
\note{
This function is only used for \code{bp}.
}
\seealso{
\code{\link{kerMat}, \link{bp}}
}

