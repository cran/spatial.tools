\name{modify_raster_margins}
\alias{modify_raster_margins}
\title{Add/subtract rows and columns from Raster*}
\usage{
modify_raster_margins(x, extent_delta = c(0, 0, 0, 0), value = NA)
}
\arguments{
  \item{x}{A Raster* object.}

  \item{extent_delta}{Numeric vector. How many rows/columns
  to add/subtract to the left,right,top, and bottom of an
  image.  Default is c(0,0,0,0) (no change).}

  \item{value}{Value to fill in when adding rows/columns.}
}
\value{
A Raster* object.
}
\description{
Add/subtract rows and columns from Raster*
}
\details{
A quick way to add/subtract margins from a Raster* object.
extent_delta is a four-element integer vector that
describes how many rows/columns to add to the
(left,right,top,bottom) of the image (in that order).
Negative values remove rows, positive values add rows.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
dim(tahoe_highrez)
# Remove one row and column from the top, bottom, left, and right:
tahoe_highrez_cropped <- modify_raster_margins(x=tahoe_highrez,extent_delta=c(-1,-1,-1,-1))
dim(tahoe_highrez_cropped)
# Add two rows to the top and left of the raster, and fill with the value 100.
tahoe_highrez_expand <- modify_raster_margins(x=tahoe_highrez,extent_delta=c(2,0,2,0),value=100)
dim(tahoe_highrez_expand)
}
\author{
Jonathan A. Greenberg
}

