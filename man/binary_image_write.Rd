\name{binary_image_write}
\alias{binary_image_write}
\title{Writes image data to a flat binary file using col/row/band positioning.}
\usage{
  binary_image_write(filename, mode = real64(), image_dims,
    interleave = "BSQ", data, data_position)
}
\arguments{
  \item{filename}{Character.  The path and filename of a
  "blank" binary file to store the image data.}

  \item{mode}{The mode of data on disk.  Defaults to
  real64() (double precision floating point).}

  \item{image_dims}{Vector. Vector of length(image_dims)==3
  representing the number of columns, rows and bands in the
  output image.}

  \item{interleave}{Character. The require output
  interleave.  By default is "BSQ". OTHER INTERLEAVES
  CURRENTLY UNSUPPORTED.}

  \item{data}{Vector, matrix, array, or other data source
  which is coercible to a vector. This is the data to be
  written to the image.}

  \item{data_position}{List. A length==3 list, containing
  the column, row, and band positions ranges to write the
  output data.}
}
\value{
  NULL
}
\description{
  Writes image data to a flat binary file using
  col/row/band positioning.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
# Create a blank file using create_blank_raster
test_blank_file <- create_blank_raster(reference_raster=tahoe_highrez)
blank_raster <- brick(test_blank_file)
# It should be all 0s:
setMinMax(blank_raster)
# Write some ones to to the 100th line, columns 25 to 50, bands 1 and 3:
data_position <- list(25:50,100,c(1,3))
data1s <- array(1,dim=c(
	length(data_position[[1]]),
	length(data_position[[2]]),
	length(data_position[[3]])))
plot(raster(test_blank_file,layer=1))
binary_image_write(filename=test_blank_file,
	mode=real64(),image_dims=dim(tahoe_highrez),interleave="BSQ",
	data=data1s,data_position=data_position)
setMinMax(blank_raster)
plot(raster(blank_raster,layer=1))
}
\author{
  Jonathan A. Greenberg
}
\seealso{
  \code{\link[mmap]{mmap}},\code{\link[spatial.tools]{create_blank_raster}}
}
\keyword{mmap}

