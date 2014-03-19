#' Model predictions (including Raster* objects)
#' @param object a model object for which prediction is desired.
#' @param ... additional arguments affecting the predictions produced.
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{predict}}
#' @details predict will operate normally, unless a parameter named "newdata"
#' is found and it is of class Raster*.  If this occurs, predict will use
#' rasterEngine to perform a prediction.  Currently, this works for predict.* 
#' statements in which the data to predict on is called by the parameter "newdata", 
#' the input data is in the form of a data.frame, and the output is a vector 
#' or matrix of numbers or factors.
#' 
#' predict will run in parallel if a cluster is registered
#' with foreach via a do* statement, or if the user uses sfQuickInit().
#'  
#' @examples
#' # This example creates a linear model relating a vegetation
#' # index (NDVI) to vegetation height, and applies it to a raster
#' # of NDVI.
#' 
#' # Load up a 3-band image:
#' tahoe_highrez <- setMinMax(
#' 		brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
#' 
#' # Determine NDVI
#' ndvi_nodrop <- function(GRNIR_image)
#' {
#' 	red_band <- GRNIR_image[,,2,drop=FALSE]
#' 	nir_band <- GRNIR_image[,,3,drop=FALSE]	
#' 	ndvi <- (nir_band-red_band)/(nir_band + red_band)
#' 	return(ndvi)
#' }
#' 
#' tahoe_ndvi <- rasterEngine(GRNIR_image=tahoe_highrez,fun=ndvi_nodrop)
#' names(tahoe_ndvi) <- "ndvi"
#' 
#' # Load up Lidar files
#' tahoe_lidar_highesthit <- setMinMax(
#' 		raster(system.file("external/tahoe_lidar_highesthit.tif", package="spatial.tools")))
#' 
#' tahoe_lidar_bareearth <- setMinMax(
#' 		raster(system.file("external/tahoe_lidar_bareearth.tif", package="spatial.tools")))
#' 
#' # Determine vegetation height:
#' LIDAR_height <- function(bareearth,firstreturn)
#' {
#' 	height <- firstreturn-bareearth
#' 	return(height)
#' }
#' 
#' tahoe_height <- rasterEngine(
#' 		bareearth=tahoe_lidar_bareearth,
#' 		firstreturn=tahoe_lidar_highesthit,
#' 		fun=LIDAR_height)
#' names(tahoe_height) <- "vegetation_height"
#' 
#' # Stack them:
#' tahoe_analysis_stack <- stack(tahoe_ndvi,tahoe_height)
#' 
#' # Pick some random points from the stack
#' randomly_extracted_data <- as.data.frame(sampleRandom(tahoe_analysis_stack,size=100))
#' 
#' # Generate a linear model from these points:
#' height_from_ndvi_model <- lm(vegetation_height~ndvi,data=randomly_extracted_data)
#' 
#' # Apply model to NDVI image:
#' # Enable parallel engine to run larger images faster:
#' # sfQuickInit()
#' height_from_ndvi_raster <- predict_rasterEngine(object=height_from_ndvi_model,newdata=tahoe_ndvi)
#' # sfQuickStop()
#' @export

predict_rasterEngine <- function(object,...)
{
	list2env(list(...),envir=environment())
	if("newdata" %in% ls())
	{
		newdata <- newdata
		if(is.Raster(newdata))
		{
			predict.rasterEngine_function <- function(newdata,object,...)
			{
				# Determine all parameters that are not newdata and object:
				local_objects <- ls()
				model_parameters <- setdiff(local_objects,c("newdata","object"))
				
				newdata_dim <- dim(newdata)
				
				predictor_names <- dimnames(newdata)[3][[1]]
				
				newdata <- aperm(newdata,c(3,1,2))
				dim(newdata) <- c(newdata_dim[3],prod(newdata_dim[1:2]))
				newdata <- t(newdata)
				
				newdata_df <- as.data.frame(newdata)
				names(newdata_df) <- predictor_names
				
				if(length(model_parameters)>0)
				{
				predict_output <- predict(object=object,newdata=newdata_df,mget(model_parameters))
				} else
				{
					predict_output <- predict(object=object,newdata=newdata_df)
				}
				
				nbands_output <- length(predict_output)/prod(newdata_dim[1:2])
				
				if(class(predict_output)=="factor")
				{
					predict_output <- as.numeric(predict_output)
				}
				
				predict_output_array <- array(predict_output,dim=c(newdata_dim[1:2],nbands_output))
				
				return(predict_output_array)
			}
			
			additional_args <- list(...)
			additional_args$newdata <- NULL
			additional_args <- c(list(object=object),unlist(additional_args,recursive=FALSE))
			
			output <- rasterEngine(newdata=newdata,fun=predict.rasterEngine_function,
					args=additional_args,.packages=(.packages()),debugmode=FALSE)
			
			return(output)
		}
	}
	return(predict(object,...))
}