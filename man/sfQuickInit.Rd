\name{sfQuickInit}
\alias{sfQuickInit}
\title{Quickly initializes a parallel cluster and registers it with foreach.}
\usage{
  sfQuickInit(cpus, methods = FALSE, ...)
}
\arguments{
  \item{cpus}{Number of cpus.  Will default to the max
  available cpus.}

  \item{methods}{Logical. Load the methods package? (if
  FALSE, faster startup). Default=FALSE.}

  \item{...}{parameters to pass to sfInit()}
}
\description{
  Quickly initializes a parallel cluster and registers it
  with foreach.
}
\details{
  (Even more) quickly start a parallel cluster with half of
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

