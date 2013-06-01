\name{spatial_sync_raster}
\alias{spatial_sync_raster}
\title{Spatially Sync Rasters}
\usage{
  spatial_sync_raster(unsynced, reference, method = "ngb",
    size_only = FALSE, raster_size, verbose = FALSE, ...)
}
\arguments{
  \item{unsynced}{A Raster object to be aligned to the
  reference raster.}

  \item{reference}{A Raster object to be used as the
  reference for syncing. Syncing will use the reference's
  projection, resolution, and extent.}

  \item{method}{Method used to compute values for the new
  RasterLayer. Either 'ngb' (nearest neighbor) or
  'bilinear' (bilinear interpolation).}

  \item{verbose}{verbose=TRUE gives feedback on the process
  (UNSUPPORTED AT PRESENT).}

  \item{size_only}{TODO}

  \item{raster_size}{TODO}

  \item{...}{parameters to be passed to writeRaster}
}
\value{
  Returns a RasterLayer, RasterBrick or RasterStack object
  synced to the reference raster object.
}
\description{
  Aligns ("syncs") a Raster to a reference Raster.
}
\details{
  Uses bilinear or nearest neighbor resampling to align a
  raster to the extent and projection of a reference raster
  and match the resolution of the reference raster.  This
  is helpful in preparing multiple files of different
  projections, resolutions, extents, and rotations for
  performing map algebra or change detection.
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}

