\name{raster_to_filenames}
\alias{raster_to_filenames}
\title{Extract filenames from all Raster* objects.}
\usage{
  raster_to_filenames(x, unique = FALSE)
}
\arguments{
  \item{x}{Raster*.  A Raster* object (even one without
  values/in memory) to determine the filename(s).}

  \item{unique}{Logical. Only return unique filenames?  If
  FALSE, one filename per layer.}
}
\value{
  Character vector of filenames.
}
\description{
  Extract filenames from all Raster* objects.
}
\details{
  This is an expansion of filename() that allows for
  RasterStacks, in-memory Raster*s, and Raster*s without
  values.  If a filename is not found, the entry will be
  "".
}
\examples{
{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
raster_to_filenames(tahoe_highrez)
raster_to_filenames(tahoe_highrez,unique=TRUE)
nodata <- raster()
raster_to_filenames(nodata)
}
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[raster]{filename}}
}

