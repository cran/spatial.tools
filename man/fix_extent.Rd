\name{fix_extent}
\alias{fix_extent}
\title{Forces a list of Raster*s to all have the same extent.}
\usage{
  fix_extent(extent_reference, broken_extents)
}
\arguments{
  \item{extent_reference}{Raster*. A Raster* object that
  will provide the extent to all the other Raster*s.  If
  unassigned, will assume it is the first Raster* in the
  broken_extents list.}

  \item{broken_extents}{list of Raster* objects. Raster*
  objects that will be coerced to the extent_reference's
  extent.}
}
\description{
  Forces a list of Raster*s to all have the same extent.
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{extent}},\code{\link{stack}}
}

