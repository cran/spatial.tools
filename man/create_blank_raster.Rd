\name{create_blank_raster}
\alias{create_blank_raster}
\title{Create an empty raster and header.}
\usage{
  create_blank_raster(filename = NULL, format = "raster",
    dataType = "FLT8S", bandorder = "BSQ", nrow = NULL,
    ncol = NULL, nlayers = NULL, create_header = TRUE,
    reference_raster = NULL, return_filename = TRUE,
    overwrite = FALSE, verbose = FALSE)
}
\arguments{
  \item{filename}{Character. The output base filename of
  the blank file.  Will use tempfile() if nothing is
  provided.}

  \item{format}{Character.  Output format.  Currently only
  supports "raster".}

  \item{dataType}{Character.  Output number type.  See
  ?dataType. Currently only supports "FLT8S".}

  \item{bandorder}{Character.  Output band interleave.
  Currently only supports "BSQ".}

  \item{nrow}{Numeric. Number of rows of the output raster.
  Defaults to nrow(reference_raster).}

  \item{ncol}{Numeric. Number of columns of the output
  raster. Defaults to ncol(reference_raster).}

  \item{nlayers}{Numeric. Number of layers of the output
  raster Defaults to nlayer(reference_raster).}

  \item{create_header}{Logical. Create a properly formatted
  header for the blank file?}

  \item{reference_raster}{Raster*. Reference raster to
  derive other information, e.g. resolution, projection,
  datum.}

  \item{return_filename}{Logical. Return filename of the
  binary file (if TRUE, default) or the Raster* itself (if
  FALSE).}

  \item{overwrite}{Logical. Overwrite an existing file with
  the same name?}

  \item{verbose}{Logical. Enable verbose execution? Default
  is FALSE.}
}
\value{
  A character vector (return_filename==TRUE) or as Raster*
  object (return_filename==TRUE)
}
\description{
  This function creates an arbitrarily large raster as a
  flat binary file with (optionally) a proper header for
  use with other functions.  This should create the blank
  files very quickly, as it is using some OS tricks to
  carve out a block of space rather than writing a bunch of
  0s to disk sequentially.
}
\details{
  create_blank_raster is designed to quickly create a
  binary file of the appropriate size using tricks with
  seek()/writeBin(). A large file can be created in a
  fraction of a second using this function. This file
  could, for example, be used with mmap to realize
  asynchronous or, OS permitting, parallel writes to a
  single file.  Note that setMinMax are NOT performed on
  the output file (to save time), so on some systems you
  may see a warning.

  Binary files of this type are used by a number of raster
  formats, including raster and ENVI.
}
\examples{
\dontrun{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
test_blank_file <- create_blank_raster(reference_raster=tahoe_highrez)
file.info(test_blank_file)
test_blank_raster <- create_blank_raster(reference_raster=tahoe_highrez,return_filename=FALSE)
test_blank_raster
}
}
\author{
  Jonathan A. Greenberg
}

