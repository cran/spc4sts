\name{spc4sts-package}
\alias{spc4sts-package}
\title{
Statistical Process Control for Stochastic Textured Surfaces
}
\description{
Provides statistical process control tools for stochastic textured surfaces. The current version supports the first tool that monitors and diagnoses for local defects on the stochastic textured surfaces, which was proposed by Bui and Apley (2017).
}
\details{
Stochastic textured surface data can be viewed as two-dimensional stochastic processes. Some examples of the stochastic textured surface data include microscopy images of material microstructure and images of stone countertops, lumber surfaces and textile fabrics. The stochastic textured surface data need not be only images. Point cloud surface roughness data of metal parts are also examples of the stochastic textured surface data.

Recent statistical process control (SPC) literature has mainly focused on profile data. Most existing works in this area required the profiles to have a gold standard, which is a meaningful profile mean function. However, stochastic textured surface data do not have a meaningful gold standard, making the SPC literature for the profile data inapplicable (see Bui and Apley 2017 for more details). Other image SPC methods that can work for the stochastic textyre surface data rely on feature extraction and are problem-specific because they require prior knowledge of the defects in order to define suitable features.

This package provides general SPC tools for the stochastic textured suface data. In this early version, the package focuses on monitoring and diagnosing general local defects that occur on some local areas on the stochastic textured surfaces. This approach was proposed by Bui and Apley (2017). For demonstration, the package includes functions to simulate stochastic textured surface images with/without local defects. For real images in a textile application, use the data package "textile".

Brief descriptions of the main functions are provided below:

\code{surfacemodel()} builds a supervised learning model (a regression tree in this version) to characterize the statistical behavior of a given stochastic textured surface data sample.

\code{localStat()} computes the monitoring statistic (that is designed for detecting local defefects) for a given image, based on the model built from \code{surfacemodel()}.

\code{climit()} establishes the control limits at the given false alarm rates from the empirical distribution of the monitoring statistics computed for a set of in-control images (i.e., without defects) using \code{localStat()}. It also constructs the diagnostic thresholds to be used for \code{diagnoseLD()}.

\code{diagnoseLD()} produces a binary diagnostic image that highlights local defects (if any) in a given stochastic textured surface image.

}
\references{
Bui, A.T., and Apley., D.W. (2017) A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics (in press).
}
\author{
Anh Bui and Daniel Apley

Maintainer: Anh Bui <atbui@u.northwestern.edu>
}
\examples{
#
# See the examples in the help pages for the main functions mentioned above.
#
}
