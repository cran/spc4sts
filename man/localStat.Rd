\name{localStat}
\alias{localStat}
\title{
Monitoring Statistic for Local Defects on Stochastic Textured Surfaces
}
\description{
Computes the spatial moving statistics (SMS), and then the monitoring Statistic for local defects on a given stochastic textured surface image.
}
\usage{
localStat(img, model, cl, stat = c("ad", "bp"), w)
}
\arguments{
  \item{img}{
the given image in the matrix format.
}
  \item{model}{
the returned object from \code{surfacemodel}
}
  \item{cl}{
the returned object from \code{climit} or \code{climit2}.
}
  \item{stat}{
the statistic used in the SMS. If \code{cl} is given, \code{stat = cl$stat}; otherwise, must be either \code{"ad"} (default) or \code{"bp"}.
}
  \item{w}{
the window size of the SMS. If \code{cl} is given, \code{w = cl$w}; otherwise, it must be an odd number >= 3.
}

}
\value{
A list of the following:
\item{sms }{a matrix of the SMS values computed for pixels in \code{img}}
\item{stat }{the argument \code{stat}}
\item{w }{the argument \code{w}}
\item{monitoringStat }{the monitoring statistic of \code{img}}
}
\references{
Bui, A.T., and Apley., D.W. (2017), "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, In press.
}
\author{
Anh Bui
}
\seealso{
\code{\link{surfacemodel}, \link{sms}, \link{dataPrep}}
}
\examples{
## build the in-control model
img <- sarGen(m = 100, n = 100, border = 50) # training image
model <- surfacemodel(img,1)

## generate an image and compute its monitoring statistic
img2 <- sarGen(m = 100, n = 100, border = 50)
ls <- localStat(img2, model, stat = "ad", w = 5)
ls$monitoringStat
}
