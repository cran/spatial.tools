\name{modis_hdf4_subdatasets}
\alias{modis_hdf4_subdatasets}
\title{Return MODIS subdataset names}
\usage{
  modis_hdf4_subdatasets(x, verbose = FALSE)
}
\arguments{
  \item{x}{Character. A MODIS HDF4 filename.}

  \item{verbose}{Logical. Enable verbose execution? Default
  is FALSE.}
}
\value{
  A character vector of subdataset names.
}
\description{
  Return MODIS subdataset names
}
\examples{
\dontrun{
 # Download a MODIS GPP tile:
 download.file(
		"http://e4ftl01.cr.usgs.gov/MOLT/MOD17A3.055/2000.01.01/
		MOD17A3.A2000001.h19v10.055.2011276104211.hdf",
		"MOD17A3.A2000001.h19v10.055.2011276104211.hdf",mode="wb")
 modis_hdf4_subdatasets("MOD17A3.A2000001.h19v10.055.2011276104211.hdf")
# This can be used in a Rgdal_translate statement by subselecting one layer:
gpp_sds <- modis_hdf4_subdatasets("MOD17A3.A2000001.h19v10.055.2011276104211.hdf")[1]
Rgdal_translate(gpp_sds,"MOD17A3.A2000001.h19v10.055.2011276104211.tif")
# This is an equivalent calling the index directly from Rgdal_translate:
Rgdal_translate("MOD17A3.A2000001.h19v10.055.2011276104211.hdf",
		"MOD17A3.A2000001.h19v10.055.2011276104211.tif",modis_sds_index=1)
}
}
\author{
  Jonathan A. Greenberg
}

