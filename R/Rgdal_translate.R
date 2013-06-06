#' Rgdal_translate
#' 
#' R wrapper for gdal_translate
#' 
#' @param src_dataset Character. The source dataset name. It can be either file name, URL of data source or subdataset name for multi-dataset files.
#' @param dst_dataset Character. The destination file name.
#' @param ot Character. ("Byte"/"Int16"/"UInt16"/"UInt32"/"Int32"/"Float32"/"Float64"/"CInt16"/"CInt32"/"CFloat32"/"CFloat64"). For the output bands to be of the indicated data type.
#' @param strict Logical. Don't be forgiving of mismatches and lost data when translating to the output format.
#' @param of Character. Select the output format. The default is GeoTIFF (GTiff). Use the short format name.
#' @param b Numeric or Character. Select an input band band for output. Bands are numbered from 1. Multiple bands may be used to select a set of input bands to write to the output file, or to reorder bands. Starting with GDAL 1.8.0, band can also be set to "mask,1" (or just "mask") to mean the mask band of the first band of the input dataset.
#' @param mask Numeric. (GDAL >= 1.8.0) Select an input band band to create output dataset mask band. Bands are numbered from 1. band can be set to "none" to avoid copying the global mask of the input dataset if it exists. Otherwise it is copied by default ("auto"), unless the mask is an alpha channel, or if it is explicitly used to be a regular band of the output dataset ("-b mask"). band can also be set to "mask,1" (or just "mask") to mean the mask band of the 1st band of the input dataset.
#' @param expand Character. ("gray"|"rgb"|"rgba").  (From GDAL 1.6.0) To expose a dataset with 1 band with a color table as a dataset with 3 (RGB) or 4 (RGBA) bands. Useful for output drivers such as JPEG, JPEG2000, MrSID, ECW that don't support color indexed datasets. The 'gray' value (from GDAL 1.7.0) enables to expand a dataset with a color table that only contains gray levels to a gray indexed dataset.
#' @param outsize Numeric. (c(xsize[%],ysize[%])). Set the size of the output file. Outsize is in pixels and lines unless '%' is attached in which case it is as a fraction of the input image size.
#' @param scale Numeric. (c(src_min,src_max,dst_min,dst_max)). Rescale the input pixels values from the range src_min to src_max to the range dst_min to dst_max. If omitted the output range is 0 to 255. If omitted the input range is automatically computed from the source data.
#' @param unscale Logical. Apply the scale/offset metadata for the bands to convert scaled values to unscaled values. It is also often necessary to reset the output datatype with the -ot switch.
#' @param srcwin Numeric. (c(xoff,yoff,xsize,ysize)).  Selects a subwindow from the source image for copying based on pixel/line location.
#' @param projwin Numeric. (c(ulx,uly,lrx,lry)).  Selects a subwindow from the source image for copying (like -srcwin) but with the corners given in georeferenced coordinates.
#' @param epo Logical. (Error when Partially Outside)  (GDAL >= 1.10) If this option is set, -srcwin or -projwin values that falls partially outside the source raster extent will be considered as an error. The default behaviour starting with GDAL 1.10 is to accept such requests, when they were considered as an error before.
#' @param eco Logical. (Error when Completely Outside) (GDAL >= 1.10) Same as -epo, except that the criterion for erroring out is when the request falls completely outside the source raster extent.
#' @param a_srs Character.  Override the projection for the output file. The srs_def may be any of the usual GDAL/OGR forms, complete WKT, PROJ.4, EPSG:n or a file containing the WKT.
#' @param a_ullr Numeric. (c(ulx,uly,lrx,lry)). Assign/override the georeferenced bounds of the output file. This assigns georeferenced bounds to the output file, ignoring what would have been derived from the source file.
#' @param a_nodata Numeric. Assign a specified nodata value to output bands. Starting with GDAL 1.8.0, can be set to none to avoid setting a nodata value to the output file if one exists for the source file
#' @param mo Character. ("META-TAG=VALUE").  Passes a metadata key and value to set on the output dataset if possible.
#' @param co Character. ("NAME=VALUE"). Passes a creation option to the output format driver. Multiple -co options may be listed. See format specific documentation for legal creation options for each format.
#' @param gcp Numeric. (c(pixel,line,easting,northing(,elevation))). Add the indicated ground control point to the output dataset. This option may be provided multiple times to provide a set of GCPs.
#' @param q Logical. Suppress progress monitor and other non-error output.
#' @param sds Logical. Copy all subdatasets of this file to individual output files. Use with formats like HDF or OGDI that have subdatasets.
#' @param stats Logical. (GDAL >= 1.8.0) Force (re)computation of statistics.
#' @param additional_commands Character. Additional commands to pass directly to gdal_translate.
#' @param modis_sds_index Numeric. If the file is a MODIS HDF4 file, which subdataset should be returned (1 to the number of subdatasets)?  If this flag is used, src_dataset should be the filename of the HDF4 file.
#' @param output_Raster Logical. Return output dst_dataset as a RasterBrick?
#' @param verbose Logical.
#' @return NULL or if(output_Raster), a RasterBrick.
#' @author Jonathan A. Greenberg (wrapper) and Frank Warmerdam (GDAL lead developer).
#' @details This is an R wrapper to the gdal_translate function that is part of the 
#' Geospatial Data Abstraction Library (GDAL) library.  It follows the parameter naming
#' conventions of the original function, with some modifications to allow for more R-like
#' parameters.  For all parameters, the user can use a single character string following,
#' precisely, the gdal_translate format (\url{http://www.gdal.org/gdal_translate.html}), or,
#' in some cases, can use R vectors to achieve the same end.  
#' 
#' This function assumes the user has a working GDAL on their system.  If the 
#' "spatial.tools.gdalInstallation" option has been set (usually by get_gdal_installation),
#' the GDAL found in that path will be used.  If nothing is found, get_gdal_installation
#' will attempt to find a working GDAL that has the right drivers as specified with the
#' "of" (output format) parameter.
#' 
#' The user can choose to (optionally) return a RasterBrick of the output file (assuming
#' raster/rgdal supports the particular output format).
#'
#' @references \url{http://www.gdal.org/gdal_translate.html}
#' @examples \dontrun{ 
#' # Example from the original gdal_translate documentation:
#' src_dataset <- system.file("external/tahoe_highrez.tif", package="spatial.tools")
#' # Original gdal_translate call:
#' # gdal_translate -of GTiff -co "TILED=YES" tahoe_highrez.tif tahoe_highrez_tiled.tif
#' # Rgdal_translate:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",co="TILED=YES",verbose=TRUE)
#' # Pull out a chunk and return as a raster:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",
#' srcwin=c(1,1,100,100),output_Raster=TRUE,verbose=TRUE)
#' # Notice this is the equivalent, but follows gdal_translate's parameter format:
#' Rgdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="Gtiff",
#' srcwin="1 1 100 100",output_Raster=TRUE,verbose=TRUE)
#' }
#' @export

Rgdal_translate <- function(src_dataset,dst_dataset,ot,strict,of="Gtiff",
		b,mask,expand,outsize,scale,unscale,srcwin,projwin,epo,eco,
		a_srs,a_ullr,a_nodata,mo,co,gcp,q,sds,stats,
		additional_commands,
		modis_sds_index,
		output_Raster=FALSE,verbose=FALSE)
{
	# Check for gdal installation
	if(is.null(getOption("spatial.tools.gdalInstallation")))
	{
		if(verbose) { message("spatial.tools.gdalInstallation not set, searching for a valid GDAL install (this may take some time)...")}
		gdal_installation <- get_gdal_installation(required_drivers=of)
	}
	
	if(is.null(getOption("spatial.tools.gdalInstallation")))
	{
		stop("GDAL with the proper drivers was not found, please check your installation.  See ?get_gdal_installation for more hints.")	
	}
	
	gdal_path <- getOption("spatial.tools.gdalInstallation")$gdal_path
	
	# Don't know if this will work on windows yet...
	base_command <- paste('"',file.path(gdal_path,"gdal_translate"),'"',sep="")
	# Don't forget to close the "'" at the end...
	
	gdal_flags <- vector()
	# Set up flags
	if(!missing(ot)) { gdal_flags <- paste(gdal_flags,paste("-ot",ot)) }
	if(!missing(strict)) { if(strict) gdal_flags <- paste(gdal_flags,"-strict") }
	if(!missing(of)) { gdal_flags <- paste(gdal_flags,paste("-of",of)) }
	if(!missing(b)) { gdal_flags <- paste(gdal_flags,paste("-b",b))	}
	if(!missing(mask)) { gdal_flags <- paste(gdal_flags,paste("-mask",mask)) }
	if(!missing(expand)) { gdal_flags <- paste(gdal_flags,paste("-expand",expand)) }
	if(!missing(outsize)) { gdal_flags <- paste(gdal_flags,paste("-outsize",paste(outsize,collapse=" "))) }
	if(!missing(scale)) { gdal_flags <- paste(gdal_flags,paste("-scale",paste(scale,collapse=" "))) }
	if(!missing(unscale)) { if(unscale) gdal_flags <- paste(gdal_flags,"-unscale") }
	print(gdal_flags)
	if(!missing(srcwin)) { gdal_flags <- paste(gdal_flags,paste("-srcwin",paste(srcwin,collapse=" "))) }
	print(gdal_flags)
	if(!missing(projwin)) { gdal_flags <- paste(gdal_flags,paste("-projwin",paste(projwin,collapse=" "))) }
	if(!missing(epo)) { if(epo) gdal_flags <- paste(gdal_flags,"-epo") }
	if(!missing(eco)) { if(eco) gdal_flags <- paste(gdal_flags,"-eco") }
	if(!missing(a_srs)) { gdal_flags <- paste(gdal_flags,paste("-a_srs",a_srs)) }
	if(!missing(a_ullr)) { gdal_flags <- paste(gdal_flags,cat("-a_ullr",a_ullr)) }
	if(!missing(a_nodata)) { gdal_flags <- paste(gdal_flags,paste("-a_nodata",a_nodata)) }
	if(!missing(mo)) { gdal_flags <- paste(gdal_flags,paste("-mo",paste("'",mo,"'",sep=""))) }
	if(!missing(co)) { gdal_flags <- paste(gdal_flags,paste("-co",paste("'",co,"'",sep=""))) }
	if(!missing(gcp)) { gdal_flags <- paste(gdal_flags,cat("-gcp",gcp)) }
	if(!missing(q)) { if(q) gdal_flags <- paste(gdal_flags,"-q") }
	if(!missing(sds)) { if(sds) gdal_flags <- paste(gdal_flags,"-sds") }
	if(!missing(stats)) { if(stats) gdal_flags <- paste(gdal_flags,"-stats") }
	if(!missing(additional_commands)) { gdal_flags <- paste(gdal_flags,additional_commands) }
		
	if(dirname(src_dataset)==".") { src_dataset <- file.path(getwd(),src_dataset) }
	if(dirname(dst_dataset)==".") { dst_dataset <- file.path(getwd(),dst_dataset) }	
	
	# MODIS HDF4 customization
	if(!missing(modis_sds_index))
	{
		src_dataset <- modis_hdf4_subdatasets(src_dataset)[modis_sds_index]
		
	}
	
	gdal_translate_cmd <- paste(paste(base_command,gdal_flags,sep=""),paste('"',src_dataset,'"',sep=""),paste('"',dst_dataset,'"',sep=""))
	
	if(verbose) message(paste("GDAL command being used:",gdal_translate_cmd))
	
	gdal_translate_output <- system(gdal_translate_cmd,intern=TRUE) 
	
	
	if(verbose) { message(gdal_translate_output) } 
	
	# (Optional) return Raster
	if(output_Raster)
	{
		return(brick(dst_dataset))	
	} else
	{
		return(NULL)
	}	
}