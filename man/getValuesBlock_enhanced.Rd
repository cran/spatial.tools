\name{getValuesBlock_enhanced}
\alias{getValuesBlock_enhanced}
\title{Easier-to-use function for grabbing a block of data out of a Raster*.}
\usage{
  getValuesBlock_enhanced(x, r1 = 1, r2 = nrow(x), c1 = 1,
    c2 = ncol(x), lyrs = seq(nlayers(x)), format = "array",
    ...)
}
\arguments{
  \item{x}{Raster* Some input Raster* object.}

  \item{r1}{Numeric. The start row of the chunk.}

  \item{r2}{Numeric. The end row of the chunk.}

  \item{c1}{Numeric. The start column of the chunk.}

  \item{c2}{Numeric. The end row of the chunk.}

  \item{lyrs}{Numeric. Vector of layer IDs.  Defaults to
  all layers (1:nlayers(x)).}

  \item{format}{Character. If "array" (default), the chunk
  will be returned in a 3-d array with dimensions
  representing column,row,and layer.  If "raster", the
  chunk will be returned as a Raster* object.}

  \item{...}{Other parameters.}
}
\value{
  An array or raster object.
}
\description{
  Easier-to-use function for grabbing a block of data out
  of a Raster*.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
mychunk <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50)
class(mychunk)
dim(mychunk)
mychunk_raster <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50,format="raster")
mychunk_raster
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[raster]{getValues}}
}

