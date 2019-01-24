\name{spc4sts-package}
\alias{spc4sts-package}
\alias{spc4sts}
\title{
Statistical Process Control for Stochastic Textured Surfaces
}
\description{
Provides statistical process control tools for stochastic textured surfaces. Some tools in the package can also be used in non-SPC contexts that deal with stochastic textured surface images (see Section Details below). The current version supports the following tools:

(1) generic modeling of stochastic textured surfaces (Bui and Apley 2018a, 2018b)

(2) local defect monitoring and diagnostics in stochastic textured surfaces (Bui and Apley 2018a)

(3) global change monitoring in the nature of stochastic textured surfaces (Bui and Apley 2018b)

(4) computation of dissimilarity matrix of stochastic textured surface images (Bui and Apley 2019b).
}
\details{
Stochastic textured surface (STS) is the term used in Bui and Apley (2018a) to refer to a class of measurement data of material surfaces that have no distinct features other than stochastic characteristics that vary randomly. A few examples of STS data include microscopy images of material microstructure samples and images of lumber surfaces, engineered stone countertops, ceramic capacitor surfaces, and textile materials that show weave patterns (Bui and Apley 2017a, 2017b, 2019a).

For STS data, even of the same nature, each image is completely different from others on a pixel-by-pixel basis. In addition, it is not straightforward to align, transform, or warp them into a common "gold standard" image, as a basis for comparison. The existence of a gold standard is a fundamental requirement for most of the statistical process control (SPC) literature for profile and multivariate data that are not STSs. An example of a gold standard in non-STS data is an image of a circuit assembly with perfectly positioned chips, to which images of actual assemblies with chips positioned inaccurately are compared for SPC quality control purposes. Existing SPC methods that may be applicable to STS data rely on some form of feature extraction from the STS images (e.g., a specific frequency component from a spectral analysis of the image), but they are problem specific because prior knowledge of abnormal behavior is needed to define suitable features.

The \pkg{spc4sts} (Statistical Process Control for Stochastic Textured Surfaces) package is the first implementation of the methods in Bui and Apley (2018a), Bui and Apley (2018b), and Bui and Apley (2019), and serves as the first off-the-shelf toolkit for performing SPC for general STS data without prior knowledge of abnormal behavior. The package is applicable to a wide range of materials as mentioned above, including random heterogeneous materials.

Some tools in the package can also be used in non-SPC contexts that deal with STS images. First, the STS modeling tool can be used in STS image characterization and reconstruction (e.g., powder materials micrograph characterization and materials microstructure image reconstruction). Second, the surface dissimilarity calculation tool can be used for STS image classification, clustering, and outlier detection. Some examples are medical microscopy image classification, cancer tissue image clustering, and outlying mammalian cell image detection.

Brief descriptions of the main functions of the package are provided below:

\code{surfacemodel()} builds a supervised learning model (a regression tree in this version) to characterize the statistical behavior of the given stochastic textured surface data sample.

\code{monitoringStat()} computes the monitoring statistic(s) (for local defects and/or global changes) for the given image, based on the model built from \code{surfacemodel()}.

\code{climit()} establishes the control limits (for local defects and/or global changes) at the given false alarm rates based on the monitoring statistics (for local defects and/or global changes) computed for a set of in-control images (i.e., without local defects or global changes) using \code{monitoringStat()}. It also constructs the diagnostic thresholds (for diagnosing local defects) to be used for \code{diagnoseLD()}.

\code{diagnoseLD()} produces a binary diagnostic image that highlights local defects (if any) in the given stochastic textured surface image.

\code{disMat()}: computes KL and/or AKL dissimilarity matrices for the given stochastic textured surface images.
}

\references{
Bui, A.T., and Apley, D.W. (2017a), textile: Textile Images, R package version 0.1.2. https://cran.r-project.org/package=textile.

Bui, A.T., and Apley, D.W. (2017b), Textile Images 2, Mendeley Data, v1. http://dx.doi.org/10.17632/wy3pndgpcx.1.

Bui, A.T. and Apley., D.W. (2018a) "A Monitoring and Diagnostic Approach for Stochastic Textured Surfaces", Technometrics, 60, 1-13.

Bui, A.T. and Apley, D.W. (2018b) "Monitoring for changes in the nature of stochastic textured surfaces", Journal of Quality Technology, 50, 363-378.

Bui, A.T. and Apley D.W. (2019a) “Textile Image 3”, figshare, http://dx.doi.org/10.6084/m9.figshare.7619351.v1.

Bui, A.T. and Apley, D.W. (2019b) "An exploratory analysis approach for understanding variation in stochastic textured surfaces", under review.
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
