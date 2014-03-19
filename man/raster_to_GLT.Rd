\name{raster_to_GLT}
\alias{raster_to_GLT}
\title{Creates a geometry lookup (GLT) file from a raster}
\usage{
raster_to_GLT(x)
}
\arguments{
  \item{x}{Raster* The raster containing the geographic
  information to be used as the basis for the GLT.}
}
\description{
Creates a geometry lookup (GLT) file from a raster
}
\details{
This function produces a two-band brick where the pixel
values for the first band are the column numbers, and the
pixel values for the second band are the row numbers of the
corresponding pixels in the input Raster* file.
}
\examples{
tahoe_lidar_highesthit <-
	raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))
tahoe_lidar_highesthit_glt <- raster_to_GLT(tahoe_lidar_highesthit)
plot(tahoe_lidar_highesthit_glt)
setMinMax(tahoe_lidar_highesthit_glt)
}
\author{
Jonathan A. Greenberg
}
\seealso{
\code{\link[raster]{rowColFromCell}},
\code{\link[raster]{cellFromRow}}
}

