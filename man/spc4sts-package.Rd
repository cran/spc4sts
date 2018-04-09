\name{spc4sts-package}
\alias{spc4sts-package}
\alias{spc4sts}
\title{
Statistical Process Control for Stochastic Textured Surfaces
}
\description{
Provides statistical process control tools for stochastic textured surfaces. The current version supports the following tools:

(1) monitors and diagnoses for local defects on the stochastic textured surfaces, which was proposed by Bui and Apley (2018a)

(2) monitors for global changes in the nature of the stochastic textured surfaces, which was proposed by Bui and Apley (2018b)
}
\details{
Stochastic textured surface data can be viewed as two-dimensional stochastic processes. Some examples of the stochastic textured surface data include microscopy images of material microstructure and images of stone countertops, lumber surfaces and textile fabrics with weave patterns. The stochastic textured surface data need not be images. Point cloud surface roughness data of metal parts are also examples of the stochastic textured surface data.

Recent statistical process control (SPC) literature has mainly focused on profile data. Most existing works in this area require the profiles to have a gold standard, which is a meaningful profile mean function. However, stochastic textured surface data do not have gold standards, making the SPC literature for the profile data inapplicable (see Bui and Apley 2018a for more details). Other SPC methods that may be applicable to STS data rely on some type of feature extraction from the STS images, but they are problem specific because prior knowledge of the defects/changes is needed in order to define suitable features.

This package provides general SPC tools for the stochastic textured suface data without requiring advanced knowledge of defects/changes. In this version, the package provides the following tools.

The first tool focuses on monitoring and diagnosing general local defects that occur on local regions on the stochastic textured surfaces. This approach was proposed in Bui and Apley (2018a). For demonstration, the package includes functions to simulate stochastic textured surface images with/without local defects. For real images in a textile application, the data package "textile" on CRAN can be used.

The second tool focuses on monitoring for general global changes that affect the entire nature of the stochastic textured surfaces, as opposed to changes that occur in just some local regions. This approach was proposed in Bui and Apley (2018b). For illustration, the functions for simulating stochastic textured surface images mentioned above can be used. The global changes can be obtained via changing parameters of the simulated surfaces. For real images in a textile application, the R data packages "textile" of Bui and Apley (2017a), which has images with and without local defects, can also be used. There is also a much larger textile image data set provided in the R data package "textile2" of Bui and Apley (2017b), but this data set only contain images without local defects.

Brief descriptions of the main functions are provided below:

\code{surfacemodel()} builds a supervised learning model (a regression tree in this version) to characterize the statistical behavior of the given stochastic textured surface data sample.

\code{monitoringStat()} computes the monitoring statistic(s) (for local defects and/or global changes) for the given image, based on the model built from \code{surfacemodel()}.

\code{climit()} establishes the control limits (for local defects and/or global changes) at the given false alarm rates based on the monitoring statistics (for local defects and/or global changes) computed for a set of in-control images (i.e., without local defects or global changes) using \code{monitoringStat()}. It also constructs the diagnostic thresholds (for diagnosing local defects) to be used for \code{diagnoseLD()}.

\code{diagnoseLD()} produces a binary diagnostic image that highlights local defects (if any) in the given stochastic textured surface image.
}

\references{
Bui, A.T., and Apley, D.W. (2017a), textile: Textile Images, R package version 0.1.2. https://cran.r-project.org/package=textile.

Bui, A.T., and Apley, D.W. (2017b), Textile Images 2, Mendeley Data, v1. http://dx.doi.org/10.17632/wy3pndgpcx.1.

Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, to appear.
}
\author{
Anh Tuan Bui and Daniel W. Apley

Maintainer: Anh Tuan Bui <atbui@u.northwestern.edu>
}
\examples{
#
# See the examples in the help pages for the main functions mentioned above.
#
}
