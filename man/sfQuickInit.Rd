\name{sfQuickInit}
\alias{sfQuickInit}
\title{Quickly initializes a parallel snowfall cluster and registers it with foreach.}
\usage{
  sfQuickInit(cpus, ...)
}
\arguments{
  \item{cpus}{Number of cpus.  Will default to the max
  available cpus.}

  \item{...}{parameters to pass to sfInit()}
}
\description{
  Quickly initializes a parallel snowfall cluster and
  registers it with foreach.
}
\details{
  (Even more) quickly start a snowfall cluster with maximum
  available cpus, parallel = TRUE, and type = "SOCK" and
  registers it with foreach.
}
\examples{
sfQuickInit(cpus=2)
sfQuickStop()
}
\author{
  Jonathan A. Greenberg
}

