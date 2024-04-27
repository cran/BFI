\name{BFI-internal}
\title{Internal BFI Functions}
\alias{A.l.maker}
\alias{negloglik.beta.sigma}
\alias{model.maker}
\description{Internal BFI functions.}
\value{

\code{A.l.maker()} returns a curvature matrix for each local.

\code{negloglik.beta.sigma()} returns the negative log-likelihood of regression coefficients and error variance, which can be utilized for optimization purposes.

\code{model.maker()} returns the outcome and design matrix, incorporating dummy variables when categorical covariates are present.
}
\details{These functions are not intended for use by users.}
\author{
Hassan Pazira\cr
Maintainer: Hassan Pazira \email{hassan.pazira@radboudumc.nl}
}
\keyword{internal}
