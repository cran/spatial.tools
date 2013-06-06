\name{get_gdal_installation}
\alias{get_gdal_installation}
\title{Find GDAL installation}
\usage{
  get_gdal_installation(return_drivers = TRUE,
    return_python_utilities = TRUE,
    return_most_current = TRUE, required_drivers = NULL,
    setOptions = TRUE, verbose = FALSE)
}
\arguments{
  \item{return_drivers}{Logical. Return a table of GDAL
  drivers? Default is TRUE.}

  \item{return_python_utilities}{Logical. Return a vector
  of available python utilities? Default is TRUE.}

  \item{return_most_current}{Logical. Return only the most
  current version of GDAL (if multiple installs are
  present)? Default is TRUE.}

  \item{required_drivers}{Character. What driver is
  required?  Default is no required drivers.}

  \item{setOptions}{Logical. Set the
  "spatial.tools.gdalInstallation" option for use with
  other functions?  Default is TRUE.}

  \item{verbose}{logical. Enable verbose execution? Default
  is FALSE.}
}
\value{
  A list with one element per GDAL installation.  See
  Description for parameter names.
}
\description{
  Tools for properly locating and determing the state of
  the Geospatial Data Abstraction Library
}
\details{
  get_gdal_installation is designed to help determine the
  correct path to the Geospatial Data Abstraction Library,
  even if the PATH is not set on the local computer.  It
  accomplishes this by brute-force searching for the
  gdalinfo(.exe) executable on the user's local system,
  starting at the root level ("/" on Unix-alikes, "C:" on
  Windows).  It can also (optionally) return information on
  the available drivers, python utilities, and can
  determine the "best" (most current version) of GDAL if
  there are multiple installs (more common on Windows boxes
  than Unix-alikes).

  Each user's installation will be different, but in the
  author's experience, for each OS, we recommend the
  following installations:

  Unix: See \url{http://gdal.org} for building/installing
  GDAL on your system.

  Mac: Install the William Kyngesburye's GDAL Complete
  Framework:
  \url{http://www.kyngchaos.com/software:frameworks}

  Windows: Several choices (in order of the author's
  preference): \itemize{ \item Standalone QGIS Installer:
  \url{http://hub.qgis.org/projects/quantum-gis/wiki/Download#11-Standalone-Installer-recommended-for-new-users}
  \item OSGeo4W (follow the "Quick Start for OSGeo4W
  Users"): \url{http://trac.osgeo.org/osgeo4w/} \item
  FWTools, 32-bit only: \url{http://fwtools.maptools.org/}
  }
}
\examples{
\dontrun{
# Determine the most current GDAL installations:
mygdals <- get_gdal_installation()
mygdals[[1]]$gdal_path
mygdals[[1]]$version
mygdals[[1]]$drivers
mygdals[[1]]$python_utilities
# Determine all available GDAL installations:
mygdals <- get_gdal_installation(return_most_current=FALSE)
sapply(mygdals,function(X) X$gdal_path)
# Only return GDAL installs that support a given driver:
mygdals <- get_gdal_installation(required_drivers="HDF4")
}
}
\author{
  Jonathan A. Greenberg and Matteo Mattiuzzi
}
\references{
  \url{http://gdal.org}
}
\keyword{format}

