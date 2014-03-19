\name{build_raster_header}
\alias{build_raster_header}
\title{Builds a raster header for a flat binary file.}
\usage{
build_raster_header(x_filename, reference_raster, out_nlayers,
  dataType = "FLT8S", format = "raster", bandorder = "BSQ",
  setMinMax = FALSE, verbose = FALSE)
}
\arguments{
  \item{x_filename}{Character. The filename of the input
  binary file.}

  \item{reference_raster}{Raster*. A Raster* object
  containing the header information to be used.}

  \item{out_nlayers}{Numeric. The number of layers in the
  flat binary file (defaults to
  nlayers(reference_raster)).}

  \item{dataType}{Character. The dataType of the flat
  binary file.  See ?dataType for available datatypes.
  Default is 'FLT8S'.}

  \item{bandorder}{Character. The bandorder
  ('BIP','BIL','BSQ') of the file. Default is 'BSQ'.}

  \item{format}{Character. The format of the header.  See
  ?hdr for valid entries.  Default is 'raster'.  CURRENTLY
  UNSUPPORTED.}

  \item{setMinMax}{Logical. Set the min/max for the file
  (will take longer to execute)?  Default=FALSE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\description{
Builds a raster header for a flat binary file.
}
\examples{
\dontrun{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
test_blank_file <- create_blank_raster(filename=paste(tempfile(),".gri",sep=""),
	reference_raster=tahoe_highrez,nlayers=2,
	create_header=FALSE,format="raster",dataType="FLT8S",bandorder="BSQ")
test_blank_raster <- build_raster_header(x_filename=test_blank_file,
	reference_raster=tahoe_highrez,out_nlayers=2,
	dataType='FLT8S',format='raster',bandorder="BSQ",setMinMax=TRUE)
test_blank_raster
}
}
\author{
Jonathan A. Greenberg and Robert Hijimans
(\email{spatial.tools@estarcion.net})
}
\seealso{
\code{\link{hdr}},\code{\link{dataType}}
}

