\name{getValuesBlock_stackfix}
\alias{getValuesBlock_stackfix}
\title{Get a block of raster cell values (optimization fix for RasterStacks)}
\usage{
  getValuesBlock_stackfix(x, row = 1, nrows = 1, col = 1,
    ncols = (ncol(x) - col + 1), lyrs = (1:nlayers(x)))
}
\arguments{
  \item{x}{Raster* object}

  \item{row}{positive integer. Row number to start from,
  should be between 1 and nrow(x)}

  \item{nrows}{postive integer. How many rows? Default is
  1}

  \item{col}{postive integer. Column number to start from,
  should be between 1 and ncol(x)}

  \item{ncols}{postive integer. How many columns? Default
  is the number of colums left after the start column}

  \item{lyrs}{integer (vector). Which layers? Default is
  all layers (1:nlayers(x))}
}
\value{
  matrix or vector (if (x=RasterLayer), unless
  format='matrix')
}
\description{
  A faster version of getValuesBlock for RasterStack.
}
\details{
  In certain cases, getValuesBlock may run very slowly on a
  RasterStack, particularly when the RasterStack is
  comprised of RasterBricks.  This code attempts to fix the
  inefficiency my running the extract on each unique file
  of the RasterStack, rather than each unique layer.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
tahoe_highrez_stack <- stack(tahoe_highrez,tahoe_highrez,tahoe_highrez)
# getValuesBlock stack extraction:
system.time(tahoe_highrez_extract <- getValuesBlock(tahoe_highrez_stack))
# getValuesBlock_stackfix stack extraction:
system.time(tahoe_highrez_extract <- getValuesBlock_stackfix(tahoe_highrez_stack))
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[raster]{getValuesBlock}}
}

