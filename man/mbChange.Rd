\name{mbChange}
\alias{mbChange}
\title{
Matchbox Change
}
\description{
Modifies a given image to have a matchbox change, which is used in Bui and Apley (2017).
}
\usage{
mbChange(img, alpha = 1)
}
\arguments{
  \item{img}{
the image to be matchboxed
}
  \item{alpha}{
the amount of matchboxing
}
}
\details{
Each column \code{i} of \code{img} is modified as follows: \code{img[2:nrow(img),i] <- (1 - alpha*(i-1)/ncol(img))*img[2:nrow(img),i] + alpha*(i-1)/ncol(img)*img[1:(nrow(img)-1),i]}
}
\value{
The matchboxed image in the matrix format.
}
\references{
Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, to appear.
}
\author{
Anh Bui
}

