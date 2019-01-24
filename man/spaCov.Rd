\name{spaCov}
\alias{spaCov}
\title{
Spatial Weighted Covariance
}
\description{
Computes the spatial weighted covariance of a pair of pixels in a given image.
}
\usage{
spaCov(img, i1, i2, j1, j2, K)
}
\arguments{
  \item{img}{
the given image
}
  \item{i1}{
the row index of the first pixel in the pair.
}
  \item{i2}{
the column index of the first pixel in the pair.
}
  \item{j1}{
the row index of the second pixel in the pair.
}
  \item{j2}{
the column index of the second pixel in the pair.
}
  \item{K}{
the weighted matrix.
}
}
\value{
The spatial weighted covariance.
}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.
}
\author{
Anh Bui
}
\seealso{
\code{\link{kerMat}, \link{bp}}
}

