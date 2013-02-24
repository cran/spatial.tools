\name{sfQuickStop}
\alias{sfQuickStop}
\title{Quickly stops a parallel snowfall cluster and deregisters it from foreach.}
\usage{
  sfQuickStop(...)
}
\arguments{
  \item{...}{parameters to pass to sfStop()}
}
\description{
  Quickly stops a parallel snowfall cluster and deregisters
  it from foreach.
}
\details{
  (Even more) quickly stop a snowfall cluster and sets
  foreach back to sequential mode via registerDoSEQ().
}
\examples{
sfQuickInit(cpus=2)
sfQuickStop()
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[snowfall]{sfInit}}
}

