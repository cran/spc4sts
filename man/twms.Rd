\name{twms}
\alias{twms}
\title{
Time-Weighted Moving Statistic
}
\description{
Computes time-weighted moving statistics EWMA or tabular CUSUM
}
\usage{
twms(x, type = c("ewma", "cusum"), lambda, mu0, K, x0 = 0)
}
\arguments{
  \item{x}{
the vector of observations to compute the time-weighted moving statistic for.
}
  \item{type}{
the type of statistic used in the computation.
}
  \item{lambda}{
the parameter of EWMA
}
  \item{mu0}{
the mean of the observations
}
  \item{K}{
the parameter of tabular CUSUM
}
  \item{x0}{
the starting value for the time-weighted moving statistics.
}
}
\value{
the EWMA or tabular CUSUM statistics
}

\author{
Anh Bui
}

\examples{
z <- twms(1:10, "ewma", lambda=0.2)
C <- twms(1:10, "cusum", mu0=5, K=1)
}
