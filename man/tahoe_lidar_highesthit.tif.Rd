\docType{data}
\name{tahoe_lidar_highesthit.tif}
\alias{tahoe_lidar_highesthit.tif}
\title{Lidar-derived highest hit (aka canopy) digital elevation model from the Lake Tahoe Basin.}
\description{
  Lidar-derived highest hit (aka canopy) digital elevation
  model from the Lake Tahoe Basin.
}
\examples{
tahoe_lidar_highesthit <- raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools"))
plot(tahoe_lidar_highesthit)
}
\author{
  Jonathan A. Greenberg \email{spatial.tools@estarcion.net}
}
\keyword{data}
\keyword{datasets}

