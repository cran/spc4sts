\name{climit.object}
\alias{climit.object}
\title{
Control Limit and Diagnostic Threshold Construction Object
}
\description{
Tthe object returned by \code{climit} or \code{climit2}.
}
\value{
\item{type }{the \code{type} argument of \code{climit}.}

\item{localStat}{contains values for local defect monitoring:
\code{nDmaxSms} is a vector that stores the \code{(nD*N + 1)} largest SMS values computed for all \code{N} Phase I images.
\code{PIstats} is a vector that stores the monitoring statistics computed for all the Phase I images.
\code{diagnostic.threshold } is a scalar/vector that stores the established diagnostic threshold(s).
\code{stat} and \code{w} are the \code{stat} and \code{w} arguments of the \code{climit} function.
\code{control.limit} is a scalar/vector that stores the established control limit(s).
}

\item{globalStat}{contains values for global change monitoring:
\code{PIstats} is a vector that stores the monitoring statistics computed for all the Phase I images.
\code{xval} is the \code{xval} argument of the \code{climit} function.
\code{a, b,} and \code{k} are the first three sample central moments of the global change monitoring statistics computed for the set of Phase I images (see Bui and Apley 2017b). These parameters are used to approximate the empirical distribution of \code{PIstats}.
\code{control.limit.trans_chi2} and \code{control.limit.ecdf} are a scalar/vector that stores the established control limit(s) using the parametric approximation of the empirical distributions and the empirical distributions directly, respectively. The former is recommended when the number of Phase I images is not enough for using directly the empirical distribution.
}

}
\references{
Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, to appear.
}
\author{
Anh Bui
}
\seealso{
\code{\link{climit}}
}
