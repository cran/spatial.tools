\name{focal_hpc}
\alias{focal_hpc}
\title{Engine for performing fast, easy to develop pixel and focal raster calculations with parallel processing capability.}
\usage{
  focal_hpc(x, fun, args = NULL, window_dims = c(1, 1),
    window_center = c(ceiling(window_dims[1]/2), ceiling(window_dims[2]/2)),
    filename = NULL, overwrite = FALSE,
    outformat = "raster", chunk_format = "array",
    minblocks = "max", blocksize = NULL, verbose = FALSE)
}
\arguments{
  \item{x}{Raster*. A Raster* used as the input into the
  function.  Multiple inputs should be stacked together.}

  \item{fun}{function. A focal function to be applied to
  the image. See Details.}

  \item{args}{list. Arguments to pass to the function (see
  ?mapply).  Note that the 'fun' should explicitly name the
  variables.}

  \item{window_dims}{Vector. The size of a processing
  window in col x row order.  Be default, a single pixel
  (c(1,1).}

  \item{window_center}{Vector. The local coordinate of the
  center of a processing window.  By default the middle of
  the processing window.  UNSUPPORTED.}

  \item{chunk_format}{Character. The format to send the
  chunk to the function.  Can be "array" (default) or
  "raster".}

  \item{minblocks}{Numeric. The minimum number of chunks to
  divide the raster into for processing.  Defaults to 1.}

  \item{blocksize}{Numeric. The size (in rows) for a block
  of data.  If unset, focal_hpc will attempt to figure out
  an optimal blocksize.}

  \item{filename}{character. Filename of the output
  raster.}

  \item{outformat}{character. Outformat of the raster. Must
  be a format usable by hdr(). Default is 'raster'.
  CURRENTLY UNSUPPORTED.}

  \item{overwrite}{logical. Allow files to be overwritten?
  Default is FALSE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\description{
  Engine for performing fast, easy to develop pixel and
  focal raster calculations with parallel processing
  capability.
}
\details{
  focal_hpc is designed to execute a function on a Raster*
  object using foreach, to achieve parallel reads,
  executions and writes. Parallel random writes are
  achieved through the use of mmap, so individual image
  chunks can finish and write their outputs without having
  to wait for all nodes in the cluster to finish and then
  perform sequential writing.  On Windows systems, random
  writes are possible but apparently not parallel writes.
  focal_hpc solves this by trying to write to a portion of
  the image file, and if it finds an error (a race
  condition occurs), it will simply retry the writes until
  it successfully finishes.  On a Linux system, truly
  parallel writes should be possible.

  focal_hpc operates in two modes, which have different
  input and outputs to the function:

  Pixel based processing:

  1) If chunk_format=="array" (default), the input to the
  function should assume an array of dimensions x,y,z where
  x = the number of columns in a chunk, y = the number of
  rows in the chunk, and z = the number of bands in the
  chunk.  If chunk_format=="raster", the input to the
  function will be a raster subset. Note that we are
  ordering the array using standards for geographic data,
  (columns, rows, bands), not how R usually thinks of
  arrays (rows, columns, bands).

  2) The output of the function should always be an array
  with the x and y dimensions matching the input, and an
  arbitrary number of band outputs.  Remember to order the
  dimensions as columns, rows, bands (x,y,z).

  Local window processing:

  1) The function should be written to process a SINGLE
  window at a time, given the dimensions of window_dims, so
  the input to the function should assume a window of
  dimensions window_dims with a local center defined by
  window_center.  As with before, the input can be passed
  to the function as an array (suggested) or a small
  raster.

  2) The output should be a single pixel value, so can
  either be a single value, or a vector (which is assumed
  to be multiple bands of a single pixel).

  The speed of the execution when running in parallel will
  vary based on the specific setup, and may, indeed, be
  slower than a sequential execution (e.g. with calc() ),
  particularly on smaller files.  Note that by simply
  running sfQuickStop(), focal_hpc will run in sequential
  mode.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
# Pixel-based processing:
	ndvi_function <- function(x,...)
	{
		# Note that x is received by the function as a 3-d array:
		red_band <- x[,,2]
		nir_band <- x[,,3]
		ndvi <- (nir_band - red_band)/(nir_band + red_band)
		# The output of the function should also be a 3-d array,
		# even if it is a single band:
		ndvi <- array(ndvi,dim=c(dim(x)[1],dim(x)[2],1))
		return(ndvi)
	}

	sfQuickInit(cpus=2)
 tahoe_ndvi <- focal_hpc(x=tahoe_highrez,fun=ndvi_function)
	sfQuickStop()

\dontrun{
# Focal-based processing:
local_smoother <- function(x,...)
{
 # Assumes a 3-d array representing
	# a single local window, and return
 # a single value or a vector of values.
	smoothed <- apply(x,3,mean)
	return(smoothed)
}
# Apply the function to a 3x3 window:
sfQuickInit(cpus=2)
tahoe_3x3_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(3,3))
sfQuickStop()

# Example with 7 x 7 window in full parallel mode:
sfQuickInit()
tahoe_7x7_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(7,7))
sfQuickStop()
}
}
\author{
  Jonathan A. Greenberg
  (\email{spatial.tools@estarcion.net})
}
\seealso{
  \code{\link{foreach}}, \code{\link{mmap}},
  \code{\link{dataType}}, \code{\link{hdr}}
}

