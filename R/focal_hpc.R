# focal_hpc helper functions

focal_hpc_precheck <- function(x,window_dims,window_center,verbose)
{
	if(verbose) message("Performing pre-checks...")
	
	if(length(window_dims)==1) window_dims=c(window_dims,window_dims)	
	if(length(window_center)==1) window_center <- c(window_center,window_center)
	
	if(verbose) { message(paste("window_dims:",window_dims,sep="")) }
	if(verbose) { message(paste("window_center:",window_center,sep="")) }
	
	window_rows=window_dims[2]
	window_cols=window_dims[1]
	
	layer_names=names(x)
	
	if(any(window_dims>1))
	{
		if(verbose) message("Focal processing mode...")
		processing_mode="focal"
		processing_unit="window"
	} else
	{
		if(verbose) message("Pixel processing mode...")
		processing_mode="pixel"
		processing_unit="chunk"
	}
	
	startrow_offset=-(window_center[2]-1)
	endrow_offset=window_rows-window_center[2]
	
	return(list(window_dims=window_dims,window_center=window_center,
					window_rows=window_rows,window_cols=window_cols,
					layer_names=layer_names,
					processing_mode=processing_mode,processing_unit=processing_unit,
					startrow_offset=startrow_offset,endrow_offset=endrow_offset))
}

focal_hpc_test <- function(x,fun,window_center,window_dims,args,
		layer_names,
		startrow_offset,endrow_offset,processing_unit,chunk_format,
		verbose)
{
	if(verbose) { message("Checking the function on a small chunk of data.") }
	
	# Add additional info to the args.
	if(!is.null(args)) {
		args$window_center=window_center
		args$window_dims=window_dims
		args$layer_names=layer_names
	} else
	{
		args=list(window_center=window_center)
		args$window_dims=window_dims
		args$layer_names=layer_names
	}
	
	# We are going to pull out the first row and first two pixels to check the function...
	
	if(processing_unit=="window")
	{
		if(verbose) { message("processing_unit=window...")}
		r_check <- getValuesBlock_enhanced(x, r1=1, r2=window_dims[2], c1=1,c2=window_dims[1],
				format=chunk_format)		
	} else
	{
		# The function works on the entire chunk.
		if(verbose) { message("processing_unit=chunk...")}
		r_check <- getValuesBlock_enhanced(x, r1=1, r2=window_dims[2], c1=1,c2=ncol(x),
				format=chunk_format)
	}
	
	# Add additional info to the args.
	r_check_args=args
	r_check_args$x=r_check
	r_check_function <- do.call(fun, r_check_args)
	
	if(processing_unit=="window")
	{
		if(class(r_check_function)!="numeric")
		{
			stop("window processing units require numeric vector outputs.  Please check your function.")
		} else outbands=length(r_check_function)
	}
	
	if(processing_unit=="chunk")
	{
		if(class(r_check_function)!="array" || 
				dim(r_check_function)[1] != ncol(x)||
				dim(r_check_function)[2] != window_dims[2])
		{
			message("chunk processing units require array vector outputs.  Please check your function.")
			stop(dim(r_check_function))
		} else outbands=dim(r_check_function)[3]
	}
	if(verbose) { message(paste("Number of output bands determined to be:",outbands,sep=" ")) }
	return(outbands)
}

focal_hpc_chunk_setup <- function(x,window_dims,window_center,
		chunk_nrows,startrow_offset,endrow_offset,minblocks)
{
	nodes <- getDoParWorkers() 
#	tr=blockSize(x,chunksize=(chunk_nrows*nodes+(window_dims[2]-1))*ncol(x))
	
	tr=blockSize(x,n=nlayers(x),minrows=window_dims[2],minblocks=minblocks)
	
	if (tr$n < nodes) nodes <- tr$n
	
	tr$row2 <- tr$row + tr$nrows - 1
	
	tr$focal_row=tr$row+startrow_offset
	tr$focal_row2=tr$row2+endrow_offset
	
	tr$focal_row[tr$focal_row<1]=1
	tr$focal_row2[tr$focal_row2>nrow(x)]=nrow(x)
	
	tr$startrow_offset = startrow_offset
	tr$endrow_offset = endrow_offset
	
#	bottom_right_buffer <- window_dims-window_center
#	top_left_buffer <- window_center-c(1,1)
#	buffers <- c(top_left_buffer,bottom_right_buffer)
#	names(buffers) <- c("left","top","right","bottom")
#	tr$buffers <- buffers
	
	texture_tr=list(rowcenters=((tr$row[1]:tr$row2[1])-startrow_offset))
	texture_tr$row=texture_tr$rowcenters+startrow_offset
	texture_tr$row2=texture_tr$rowcenters+endrow_offset
	
	return(list(tr=tr,texture_tr=texture_tr))
}

focal_hpc_focal_getChunk <- function(x,tr,format,r,i,r_old,chunkArgs)
{
	# Create some blank variables:
	window_center <- NULL
	window_dims <- NULL
	
	
	list2env(chunkArgs,envir=environment())
	
	startrow_offset <- tr$startrow_offset
	endrow_offset <- tr$endrow_offset
	
	chunk_format=format
	if(i==1)
	{
		r <- getValuesBlock_enhanced(x, r1=tr$focal_row[i], r2=tr$focal_row2[i], c1=1, c2=ncol(x),
				format=chunk_format)
	} else
	{
		r <- getValuesBlock_enhanced(x, r1=(tr$focal_row2[(i-1)]+1), r2=tr$focal_row2[i], 
				c1=1, c2=ncol(x),format=chunk_format)
	}
	
	if(i==1)
	{
		# Add top cap
		if((1-(tr$row[1]+startrow_offset))>0)
			r=abind(
					array(data=NA,dim=c(ncol(x),(1-(tr$row[1]+startrow_offset)),nlayers(x))),
					r,
					along=2)
	}
	
	if(i==tr$n)
	{
		# Add bottom cap
		if(nrow(x)-tr$row2[tr$n]+endrow_offset>0)
			r=abind(r,
					array(data=NA,dim=c(ncol(x),(nrow(x)-tr$row2[tr$n]+endrow_offset),nlayers(x))),
					along=2)
	}
	
	# TODO: WE NEED TO BE ABLE TO SUBTRACT STUFF HERE ALSO (if center is outside)
	left_cap=window_center[2]-1
	right_cap=window_dims[2]-window_center[2]
	
	if(left_cap>0)
	{
		# Add left cap.
		r=abind(
				array(data=NA,dim=c(left_cap,dim(r)[2],dim(r)[3])),
				r,
				along=1)
	}
	
	if(right_cap>0)
	{
		# Add right cap.
		r=abind(
				r,
				array(data=NA,dim=c(right_cap,dim(r)[2],dim(r)[3])),
				along=1)
	}
	
	if(i>1 && window_dims[2]>1)
	{
		r <- abind(r_old,r,along=2)
	}
	return(r)	
}

focal_hpc_focalChunkFunction <- function(chunk,chunkArgs)
{	
	# Create some blank variables:
	x <- NULL
	layer_names <- NULL
	fun <- NULL
	window_dims <- NULL
	outbands <- NULL
	
	#
	e <- list2env(chunkArgs,envir=environment())
	
	window_index=1:ncol(x)
	r_out=
			array(t(
							mapply(
									function(window_index,chunk,args,window_dims)
									{
										x_array=chunk[(window_index:(window_index+window_dims[2]-1)),,]									
										dimnames(x_array) <- vector(mode="list",length=3)
										if(!is.null(layer_names)) dimnames(x_array)[[3]]=layer_names
										
										fun_args=args
										fun_args$x=x_array
										r_out <- do.call(fun, fun_args)
										return(r_out)
									}
									,
									window_index,
									MoreArgs=list(chunk=chunk$processing_chunk,args=args,window_dims=window_dims)
							)
					),dim=c(ncol(x),1,outbands))
	
	image_dims=dim(x)
	image_dims=c(image_dims[2],image_dims[1],image_dims[3])
	chunk_position=list(
			1:ncol(x),
			chunk$row_center,
			1:outbands
	)
	
	writeSuccess <- FALSE
	while(!writeSuccess)
	{
		writeSuccess=TRUE
		tryCatch(
				binary_image_write(filename=filename,mode=real64(),image_dims=image_dims,
						interleave="BSQ",data=r_out,data_position=chunk_position)
				,
				error=function(err) writeSuccess <<- FALSE)	
	}
}

focal_hpc_pixelChunkFunction <- function(chunkID,tr,x,
		chunk_format,fun,fun_args,layer_names,outbands,filename)
{
	# Read the chunk
	r <- getValuesBlock_enhanced(x,r1=tr$row[chunkID],r2=tr$row2[chunkID],
			c1=1,c2=ncol(x),format=chunk_format)
	
	fun_args$x=r
	if(chunk_format=="array")
	{
		dimnames(fun_args$x)=vector(mode="list",length=3)
		if(!is.null(layer_names)) dimnames(fun_args$x)[[3]]=layer_names
	}
	
	# Execute the function.
	r_out <- do.call(fun, fun_args)
	
	# Write the output
	image_dims=dim(x)
	image_dims=c(image_dims[2],image_dims[1],image_dims[3])
	chunk_position=list(
			1:ncol(x),
			tr$row[chunkID]:tr$row2[chunkID],
			1:outbands
	)
	
	writeSuccess=FALSE
	while(!writeSuccess)
	{
		writeSuccess=TRUE
		tryCatch(
				binary_image_write(filename=filename,mode=real64(),image_dims=image_dims,
						interleave="BSQ",data=r_out,data_position=chunk_position)
				,
				error=function(err) writeSuccess <<- FALSE)	
	}
	
}

focal_hpc_pixel_processing <- function(tr,chunkArgs)
{
	# Create some blank variables:
	x <- NULL
	chunk_format <- NULL
	fun <- NULL
	layer_names <- NULL
	outbands <- NULL
		
	list2env(chunkArgs,envir=environment())
	chunkID <- seq(tr$n)
	foreach(chunkID=chunkID, .packages=c("raster","rgdal","spatial.tools","mmap")) %dopar% 
			spatial.tools:::focal_hpc_pixelChunkFunction(chunkID,tr,x,chunk_format,fun,args,layer_names,outbands,
					filename)
}

focal_hpc_focal_processing <- function(tr,texture_tr,chunkArgs)
{
	# Create some blank variables:
	verbose <- NULL
	x <- NULL
	chunk_format <- NULL
	chunk <- NULL
	window_dims <- NULL
	
	list2env(chunkArgs,envir=environment())
	
	r_old <- NULL
	for(i in 1:tr$n)
	{
		if(verbose) cat("Iteration: ",i," of ",tr$n,"\n")
		
		r <- spatial.tools:::focal_hpc_focal_getChunk(x=x,tr=tr,format=chunk_format,i=i,r_old=r_old,
				chunkArgs=chunkArgs)
		
		# We need to divide up the chunks here.
		# This is going to cause memory issues if we aren't careful...
		j=1:tr$nrows[i]
		row_centers=tr$row[i]:tr$row2[i]
		
		#(tr$row[i]:tr$row2[i])-(tr$startrow_offset)
		chunkList=mapply(function(j,r,texture_tr,row_centers,chunk_format)
				{				
					if(chunk_format=="array")
					{
#						processing_chunk=array(data=as.vector(r[,texture_tr$row[j]:texture_tr$row2[j],]),
#								dim=c(dim(r)[1],length(texture_tr$row[j]:texture_tr$row2[j]),dim(r)[3]))
						processing_chunk=r[,texture_tr$row[j]:texture_tr$row2[j],]
					}
					if(chunk_format=="raster")
					{
						processing_chunk=getValuesBlock_enhanced(r,
								r1=texture_tr$row[j],r2=texture_tr$row2[j],
								format="raster")
						#	processing_chunk=r[,texture_tr$row[j]:texture_tr$row2[j],]
					}
					out_chunk=list(row_center=row_centers[j],
							processing_chunk=processing_chunk)
					return(out_chunk)
				}
				,j,MoreArgs=list(r=r,texture_tr=texture_tr,row_centers=row_centers,chunk_format=chunk_format),
				SIMPLIFY=FALSE)
		
#		browser()
		foreach(chunk=chunkList, .packages=c("raster","rgdal","spatial.tools","mmap")) %dopar% 
				spatial.tools:::focal_hpc_focalChunkFunction(chunk,chunkArgs)
		
		if(i<tr$n && window_dims[2] > 1)
			r_old=array(data=r[,(dim(r)[2]-(tr$focal_row2[i]-tr$focal_row[i+1])):dim(r)[2],],
					dim=c(
							dim(r)[1],
							length((dim(r)[2]-(tr$focal_row2[i]-tr$focal_row[i+1])):dim(r)[2]),
							dim(r)[3])
			)
	}
}

#' Engine for performing fast, easy to develop pixel and focal raster calculations with parallel processing capability.
#' @param x Raster*. A Raster* used as the input into the function.  Multiple inputs should be stacked together.
#' @param fun function. A focal function to be applied to the image. See Details.
#' @param args list. Arguments to pass to the function (see ?mapply).
#' @param window_dims Vector. The size of a processing window in col x row order.  Be default, a single pixel (c(1,1).
#' @param window_center Vector. The local coordinate of the center of a processing window.  By default the middle of the processing window.  UNSUPPORTED.
#' @param chunk_format Character. The format to send the chunk to the function.  Can be "array" (default) or "raster".
#' @param minblocks Numeric. The minimum number of chunks to divide the raster into for processing.  Defaults to 1.
#' @param filename character. Filename of the output raster.
#' @param outformat character. Outformat of the raster. Must be a format usable by hdr(). Default is 'raster'. CURRENTLY UNSUPPORTED.
#' @param overwrite logical. Allow files to be overwritten? Default is FALSE.
#' @param verbose logical. Enable verbose execution? Default is FALSE.  
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{foreach}}, \code{\link{mmap}}, \code{\link{dataType}}, \code{\link{hdr}} 
#' @details focal_hpc is designed to execute a function on a Raster* object using foreach, to
#' achieve parallel reads, executions and writes. Parallel random writes are achieved through the use of
#' mmap, so individual image chunks can finish and write their outputs without having to wait for
#' all nodes in the cluster to finish and then perform sequential writing.  On Windows systems,
#' random writes are possible but apparently not parallel writes.  focal_hpc solves this by trying to
#' write to a portion of the image file, and if it finds an error (a race condition occurs), it will
#' simply retry the writes until it successfully finishes.  On a Linux system, truly parallel writes
#' should be possible.
#'
#' focal_hpc operates in two modes, which have different input and outputs to the function:
#' 
#' Pixel based processing:
#' 
#' 1) If chunk_format=="array" (default), the input to the function should assume an array of dimensions 
#' x,y,z where x = the number of columns in a chunk, y = the number of rows in the chunk, and 
#' z = the number of bands in the chunk.  If chunk_format=="raster", the input to the function
#' will be a raster subset.
#' Note that we are ordering the array using standards for geographic data, (columns, rows, bands), 
#' not how R usually thinks of arrays (rows, columns, bands).
#' 
#' 2) The output of the function should always be an array with the x and y dimensions matching
#' the input, and an arbitrary number of band outputs.  Remember to order the dimensions as
#' columns, rows, bands (x,y,z).
#' 
#' Local window processing:
#' 
#' 1) The function should be written to process a SINGLE window at a time, given the dimensions
#' of window_dims, so the input to the function should assume a window of dimensions window_dims 
#' with a local center defined by window_center.  As with before, the input can be passed to 
#' the function as an array (suggested) or a small raster.
#' 
#' 2) The output should be a single pixel value, so can either be a single value, or a vector
#' (which is assumed to be multiple bands of a single pixel).
#' 
#' The speed of the execution when running in parallel will vary based on the specific setup, 
#' and may, indeed, be slower than a sequential execution (e.g. with calc() ), 
#' particularly on smaller files.  Note that by simply running sfQuickStop(), focal_hpc
#' will run in sequential mode.
#' 
#' @examples

#'  tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
#' # Pixel-based processing:
#' 	ndvi_function <- function(x,...)
#'	{
#' 		# Note that x is received by the function as a 3-d array:
#'		red_band <- x[,,2]
#'		nir_band <- x[,,3]
#'		ndvi <- (nir_band - red_band)/(nir_band + red_band)
#' 		# The output of the function should also be a 3-d array,
#' 		# even if it is a single band:
#' 		ndvi <- array(ndvi,dim=c(dim(x)[1],dim(x)[2],1))
#'		return(ndvi)
#'	}
#' 	sfQuickInit(cpus=2)
#'  tahoe_ndvi <- focal_hpc(x=tahoe_highrez,fun=ndvi_function)
#' 	sfQuickStop()
#' \dontrun{ 
#' # Focal-based processing:
#' local_smoother <- function(x,...)
#' {
#'  # Assumes a 3-d array representing
#' 	# a single local window, and return
#'  # a single value or a vector of values.
#'	smoothed <- apply(x,3,mean)
#'	return(smoothed)
#' }
#' # Apply the function to a 3x3 window:
#' sfQuickInit(cpus=2)
#' tahoe_3x3_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(3,3))
#' sfQuickStop()
#' 
#' # Example with 7 x 7 window in full parallel mode:
#' sfQuickInit()
#' tahoe_7x7_smoothed <- focal_hpc(x=tahoe_highrez,fun=local_smoother,window_dims=c(7,7))
#' sfQuickStop()
#' }
#' @export

focal_hpc <- function(x,
		fun,args=NULL, 
		window_dims=c(1,1), 
		window_center=c(ceiling(window_dims[1]/2),ceiling(window_dims[2]/2)),
		filename=NULL, overwrite=FALSE,outformat="raster",
		chunk_format="array",minblocks=1,
		verbose=FALSE) 
{
	# Required libraries:
#	require("raster")
#	require("foreach")
#	require("rgdal")
#	require("mmap")
#	require("abind")
	
	# Create some blank variables to avoid warnings:

	layer_names <- NULL
	startrow_offset <- NULL
	endrow_offset <- NULL
	processing_unit <- NULL
	chunk_nrows <- NULL
	tr <- NULL
	processing_mode <- NULL
	texture_tr <- NULL
	
	# Register a sequential backend if one is not already registered:
	if(!getDoParRegistered()) registerDoSEQ()
	
	# Prechecks.
	list2env(spatial.tools:::focal_hpc_precheck(x,window_dims,window_center,verbose),envir=environment())
	
	# Test focal_hpc and determine the number of outbands.
	outbands <- spatial.tools:::focal_hpc_test(x,fun,window_center,window_dims,args,layer_names,
			startrow_offset,endrow_offset,processing_unit,chunk_format,verbose)
	
	# Set up chunking parameters.
	list2env(spatial.tools:::focal_hpc_chunk_setup(
					x=x,window_dims=window_dims,window_center=window_center,
					chunk_nrows=chunk_nrows,startrow_offset=startrow_offset,
					endrow_offset=endrow_offset,minblocks),
			envir=environment())
	
	# Create blank image file.
	out <- create_blank_raster(filename=filename,
			format="raster",dataType="FLT8S",bandorder="BSQ",
			nlayers=outbands,
			create_header=TRUE,reference_raster=x,
			overwrite=overwrite,verbose=verbose)
	
	# Create chunk arguments.
	if(verbose) { message("Loading chunk arguments.") }
	chunkArgs = list(fun=fun,x=x,x_ncol=ncol(x),tr=tr,
			window_dims=window_dims,window_center=window_center,
			layer_names=layer_names,
			args=args,filename=out,
			outbands=outbands,processing_unit=processing_unit,
			verbose=verbose,layer_names=layer_names,
			chunk_format=chunk_format)
	
	# Processing:
	if(processing_mode=="focal")
	{
		spatial.tools:::focal_hpc_focal_processing(tr,texture_tr,chunkArgs)
	} else
	{
		# We need to create a more efficient pixel-based processor
		spatial.tools:::focal_hpc_pixel_processing(tr,chunkArgs)
	}
	
	return(brick(out))
	
}

