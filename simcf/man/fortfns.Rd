\name{fortfns}
\alias{fortify.oprobitsim}
\alias{fortify.logitsim}
\title{Fortify simcf results}
\description{
  Functions that fortify simcf results into "tidy" data.frames for ggplot2
}
\usage{
fortify.logitsim(result, counterfactual, keep=NULL)

fortify.oprobitsim(result, counterfactual, keep=NULL)
}

\arguments{
  \item{result}{list, as returned by the apppropriate simcf simulation function.}
  \item{counterfactual}{counterfactual object, as returned by cfMake or cfMake2}
  \item{keep}{string vector, manual selection of the variables that
  define unique scenarios. Default is NULL, which causes the function to
  infer these values from the counterfactual.}
}
\details{
  These functions turn the results outputted by various simcf functions
  into long-formatted data as described in
  http://vita.had.co.nz/papers/tidy-data.pdf. These can then be plotted
  relatively easily using the ggplot2 package.
}
\author{Peter Schmiedeskamp <\email{pschmied@uw.edu}>}

\keyword{design}
\keyword{manip}


