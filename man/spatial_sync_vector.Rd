\name{spatial_sync_vector}
\alias{spatial_sync_vector}
\title{Matches a vector's projection to another vector or raster object's projection.}
\usage{
  spatial_sync_vector(unsynced, reference, verbose = TRUE)
}
\arguments{
  \item{unsynced}{The vector to be projected.}

  \item{reference}{A raster or vector object who's
  projection the unsynced will be matched to.}

  \item{verbose}{Logical. Verbose logging?}
}
\description{
  Matches a vector's projection to another vector or raster
  object's projection.
}
\examples{
tahoe_highrez_training_points_utm <- readOGR(dsn=system.file("external", package="spatial.tools"),layer="tahoe_highrez_training_points_utm")
print(projection(tahoe_highrez_training_points_utm))
tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools"))
print(projection(tahoe_lidar_bareearth))
tahoe_highrez_training_points_utm_synced <- spatial_sync_vector(tahoe_highrez_training_points_utm,tahoe_lidar_bareearth)
print(projection(tahoe_highrez_training_points_utm_synced))
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}

