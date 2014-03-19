\name{rasterEngine}
\alias{rasterEngine}
\title{Engine for performing fast, easy-to-develop pixel and focal raster calculations with parallel processing capability.}
\usage{
rasterEngine(x, fun = NULL, args = NULL, window_dims = c(1, 1),
  window_center = c(ceiling(window_dims[1]/2), ceiling(window_dims[2]/2)),
  filename = NULL, overwrite = FALSE, outformat = "raster",
  additional_header = "ENVI", processing_unit = NULL,
  chunk_format = "array", minblocks = "max", blocksize = NULL,
  outbands = NULL, outfiles = NULL, setMinMax = FALSE,
  compileFunction = FALSE, debugmode = FALSE, .packages = NULL,
  verbose = FALSE, ...)
}
\arguments{
  \item{x}{Raster*. A Raster* used as the input into the
  function.  This is optional, as long as some Raster* was
  defined in "..."}

  \item{fun}{Function. A focal function to be applied to
  the image. See Details.}

  \item{args}{List. Arguments to pass to the function (see
  ?mapply).  Note that the 'fun' should explicitly name the
  variables.}

  \item{window_dims}{Vector. The size of a processing
  window in col x row order.  Be default, a single pixel
  (c(1,1).}

  \item{window_center}{Vector. The local coordinate of the
  center of a processing window.  By default the middle of
  the processing window.  CURRENTLY UNSUPPORTED.}

  \item{filename}{Character. Filename(s) of the output
  raster.}

  \item{overwrite}{Logical. Allow files to be overwritten?
  Default is FALSE.}

  \item{outformat}{Character. Outformat of the raster. Must
  be a format usable by hdr(). Default is 'raster'.
  CURRENTLY UNSUPPORTED.}

  \item{processing_unit}{Character. ("single"|"chunk") Will
  be auto-set if not specified ("chunk" for
  pixel-processing, "single" for focal processing).  See
  Details.}

  \item{chunk_format}{Character. The format to send the
  chunk to the function.  Can be "array" (default) or
  "raster".}

  \item{minblocks}{Numeric. The minimum number of chunks to
  divide the raster into for processing.  Defaults to 1.}

  \item{blocksize}{Numeric. The size (in rows) for a block
  of data.  If unset, rasterEngine will attempt to figure
  out an optimal blocksize.}

  \item{outbands}{Numeric. If known, how many bands in each
  output file?  Assigning this and outfiles will allow
  focal_hpc to skip the pre-check.}

  \item{outfiles}{Numeric. If known, how many output files?
  Assigning this and outbands will allow focal_hpc to skip
  the pre-check.}

  \item{setMinMax}{Logical. Run a setMinMax() on each
  output file after processing (this will slow the
  processing down). Default is FALSE.}

  \item{additional_header}{Character. Create additional
  output headers for use with other GIS systems (see
  \code{\link{hdr}}). Set to NULL to suppress.  Default is
  "ENVI".}

  \item{compileFunction}{Logical. Runs a byte-code
  compilation on the user function before running. Setting
  this to TRUE may speed up the execution.  Default is
  FALSE.}

  \item{debugmode}{Logical.  If TRUE, the function will
  enter debug mode during the test phase.  Note the inputs
  will be an array of size 2 columns, 1 row, and how ever
  many input bands.}

  \item{.packages}{Character. A character vector of package
  names needed by the function (parallel mode only).}

  \item{verbose}{Logical. Enable verbose execution? Default
  is FALSE.}

  \item{...}{Raster*s. Named variables pointing to Raster*
  objects.  See Details.}
}
\description{
Engine for performing fast, easy-to-develop pixel and focal
raster calculations with parallel processing capability.
}
\details{
rasterEngine is designed to execute a function on one or
multiple Raster* object(s) using foreach, to achieve
parallel reads, executions and writes. Parallel random
writes are achieved through the use of mmap, so individual
image chunks can finish and write their outputs without
having to wait for all nodes in the cluster to finish and
then perform sequential writing.  On Windows systems,
random writes are possible but apparently not parallel
writes.  rasterEngine solves this by trying to write to a
portion of the image file, and if it finds an error (a race
condition occurs), it will simply retry the writes until it
successfully finishes.  On Unix-alikes, truly parallel
writes should be possible.

rasterEngine is a convienence wrapper for
\code{\link{focal_hpc}} and, in general, should be used
instead of focal_hpc directly.

rasterEngine operates in two modes, which have different
input and outputs to the function:

Pixel based processing:

1) If chunk_format=="array" (default), the input to the
function should assume an array of dimensions x,y,z where x
= the number of columns in a chunk, y = the number of rows
in the chunk, and z = the number of bands in the chunk.  If
chunk_format=="raster", the input to the function will be a
raster subset. Note that we are ordering the array using
standards for geographic data, (columns, rows, bands), not
how R usually thinks of arrays (rows, columns, bands).

2) If a single file is to be output from rasterEngine, the
output of the function should always be an array with the x
and y dimensions matching the input, and an arbitrary
number of band outputs.  Remember to order the dimensions
as columns, rows, bands (x,y,z).

If a multiple file output is required, the output of the
function should return a list of arrays each with an
equivalent number of columns and rows.  The first element
of the list will be assigned to the first filename (if
provided).

Local window processing:

1) The function should be written to process a SINGLE
window at a time, given the dimensions of window_dims, so
the input to the function should assume a window of
dimensions window_dims with a local center defined by
window_center.  As with before, the input can be passed to
the function as an array (suggested) or a small raster.

2) The output should be a single pixel value, so can either
be a single value, or a vector (which is assumed to be
multiple bands of a single pixel).

The speed of the execution when running in parallel will
vary based on the specific setup, and may, indeed, be
slower than a sequential execution (e.g. with calc() ),
particularly on smaller files.  Note that by simply running
sfQuickStop(), rasterEngine will run in sequential mode.

A fuller tutorial is available at
\url{http://publish.illinois.edu/jgrn/software-and-datasets/rasterengine-tutorial/}
}
\examples{
# Pixel-based processing on one band:
apply_multiplier <- function(inraster,multiplier)
{
	# Note that inraster is received by this function as a 3-d array (col,row,band)
	multiplied_raster <- inraster * multiplier
	return(multiplied_raster)
}

tahoe_lidar_highesthit <-
	setMinMax(raster(system.file(
	"external/tahoe_lidar_highesthit.tif", package="spatial.tools")))

# Note that you can use any parallel backend that can be registered with foreach.
# sfQuickInit() will spawn a PSOCK cluster using the parallel package.
sfQuickInit(cpus=2)
tahoe_lidar_highesthit_multiplied <- rasterEngine(
	inraster=tahoe_lidar_highesthit,
	fun=apply_multiplier,
	args=list(multiplier=3.28084))
sfQuickStop()

\dontrun{
# Pixel-based processing on more than one band:
ndvi <- function(GRNIR_image)
{
	# The input array will have dim(GRNIR_image)[3] equal
	# to 3, because the input image has three bands.
	# Note: the following two lines return an array,
	# so we don't need to manually set the dim(ndvi) at the
	# end.  If we didn't use drop=FALSE, we'd need to
	# coerce the output into a 3-d array before returning it.
	red_band <- GRNIR_image[,,2,drop=FALSE]
	nir_band <- GRNIR_image[,,3,drop=FALSE]
	ndvi <- (nir_band - red_band)/(nir_band + red_band)
	# The following is not needed in this case:
	# dim(ndvi) <- c(dim(GRNIR_image)[1],dim(GRNIR_image)[2],1)
	return(ndvi)
}

tahoe_highrez <- setMinMax(
	brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))

sfQuickInit(cpus=2)
tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi)
sfQuickStop()

# Focal-based processing:
mean_smoother <- function(inraster)
{
	smoothed <- apply(inraster,3,mean)
	return(smoothed)
}

# Apply the function to a 3x3 window:
sfQuickInit(cpus=2)
tahoe_3x3_smoothed <- rasterEngine(inraster=tahoe_highrez,fun=mean_smoother,window_dims=c(3,3))
sfQuickStop()

# Example with 7 x 7 window in full parallel mode:
sfQuickInit()
tahoe_7x7_smoothed <- rasterEngine(inraster=tahoe_highrez,fun=mean_smoother,window_dims=c(7,7))
sfQuickStop()
}
}
\author{
Jonathan A. Greenberg (\email{spatial.tools@estarcion.net})
}
\seealso{
\code{\link{focal_hpc}}, \code{\link{foreach}},
\code{\link{mmap}}, \code{\link{dataType}},
\code{\link{hdr}}
}

