#' Model predictions (including Raster* objects)
#' @param object a model object for which prediction is desired.
#' @param filename Character. Output filename.  If unset, will default to a temporary file.
#' @param na.rm.mode Logical. Attempt to fix missing data, even if the model object doesn't support na.rm?  Default is TRUE.
#' @param gain Numeric. A multiplier on the outputs (default = 1).
#' @param offset Numeric. An additive to the output (default = 0).
#' @param ncores Numeric. Number of cores to use when predicting.  Only used with randomForestSRC for now.  Do not combine with foreach registered parallel engines (e.g. sfQuickInit())
#' @param debugmode Logical. Internal debugging for the code, will be removed eventually. Default is FALSE.
#' @param verbose Logical. Enable verbose execution? Default is FALSE.  
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
#' library("raster")
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
#' @importFrom stats complete.cases
#' @export

predict_rasterEngine <- function(object,filename=NULL,na.rm.mode=TRUE,
		gain=1,offset=0,
#		prob=NULL,
		ncores=1,debugmode=FALSE,verbose=F,...)
{
	
	prob <- NULL
	
	list2env(list(...),envir=environment())
	if("newdata" %in% ls())
	{
		newdata <- newdata
		if(is.Raster(newdata))
		{
			### FUNCTION TO BE PASSED TO RASTERENGINE
			predict.rasterEngine_function <- function(newdata,object,na.rm.mode,ncores,gain,offset,...)
			{
#				browser()
				# if(sum(newdata$dim[1:2]) > 3) browser()
				
				# Determine all parameters that are not newdata and object:
				local_objects <- ls()
				model_parameters <- setdiff(local_objects,c("newdata","object","na.rm.mode","ncores","gain","offset"))
				
#				browser()
				
				
				# Parallel processing 
				if("rfsrc" %in% class(object))
				{
					options(rf.cores = ncores)
					options(mc.cores = 1)
				}
				
				
				
				newdata_dim <- newdata$dim
				
#				if(sum(newdata_dim[1:2]) > 3) browser()
				
				newdata_df <- newdata$values
				
#				if(is.null(coercion.function))
#				{
#					newdata_dim <- dim(newdata)
#					
#					predictor_names <- dimnames(newdata)[3][[1]]
#					
#					newdata <- aperm(newdata,c(3,1,2))
#					dim(newdata) <- c(newdata_dim[3],prod(newdata_dim[1:2]))
#					newdata <- t(newdata)
#					
#					newdata_df <- as.data.frame(newdata)
#					names(newdata_df) <- predictor_names
#					
#					if(any(factor_layers))
#					{
#						
#					}
#				}
				
				if("randomForest" %in% class(object) || "rfsrc" %in% class(object)) na.rm.mode=TRUE
				
				newdata_complete <- NULL
				
				if(na.rm.mode)
				{
					newdata_complete <- complete.cases(newdata_df)
					newdata_df[is.na(newdata_df)] <- 1
				}
				
				# SPECIFIC MODEL FIXES:
				# randomForest:
				if("randomForest" %in% class(object))
				{
					# First check for complete cases:
					#	newdata_complete <- complete.cases(newdata_df)
					
					# Placeholders:
					#	newdata_df[is.na(newdata_df)] <- 0
					
					# Missing factors
					xlevels <- object$forest$xlevels
					factor_variables <- sapply(xlevels,function(x) class(x)[1] == "character")
					if(any(factor_variables))
					{
						for(i in seq(xlevels)[factor_variables])
						{
							variable_name <- names(factor_variables)[i]
							temp_data <- newdata_df[[variable_name]]
							unique_levels <- xlevels[[i]]
							#	levels(temp_data) <- unique_levels
							missing_factor_ids <- temp_data %in% unique_levels
							newdata_complete <- newdata_complete & missing_factor_ids 
							# Place a value as a placeholder:
							newdata_df[!missing_factor_ids,i] <- unique_levels[1]
							newdata_df[[variable_name]] <- droplevels(newdata_df[[variable_name]])
							levels(newdata_df[[variable_name]]) <- unique_levels
						}
					}
				}
				
				if(length(model_parameters)>0)
				{
					predict_output <- predict(object=object,newdata=newdata_df,mget(model_parameters))
				} else
				{
					# Fix for rfsrc quantileReg:
					if(!is.null(prob))
					{
#						browser()
						predict_output <- predict(object=object,prob=prob,newdata=newdata_df)
		#						predict_output <- randomForestSRC::quantileReg(obj=object,prob=prob,newdata=newdata_df)
					} else
					{
						predict_output <- predict(object=object,newdata=newdata_df)
					}
				}
				
				# This needs to be made more "secure"
				# Fix for rfsrc:
				if("rfsrc" %in% class(predict_output))
				{
					if(predict_output$family != "class")
					{
						# New fix for multivariate forests:
						if(predict_output$family == "regr+")
						{
							predict_output <- sapply(predict_output$regrOutput,function(x) return(x$predicted)) 
						}else
						{
							predict_output <- predict_output$predicted
						}
					} else
					{
						predict_output <- predict_output$class	
					}
				}	
				
#				# Fix for quantregForest:
#				if("quantregForest" %in% class(predict_output))
#				{
#					if(predict_output$family != "class")
#					{
#						# New fix for multivariate forests:
#						if(predict_output$family == "regr+")
#						{
#							predict_output <- sapply(predict_output$regrOutput,function(x) return(x$predicted)) 
#						}else
#						{
#							predict_output <- predict_output$predicted
#						}
#					} else
#					{
#						predict_output <- predict_output$class	
#					}
#				}	
				
				
				# Not sure if this will work with =="array"...
				if(class(predict_output)[1]=="numeric" || class(predict_output)[1]=="factor" || is.array(predict_output))
				{
					#	dim(predict_output) <- c(newdata_dim[1:2])
					predict_output <- as.data.frame(predict_output)
				}
				
				nbands_output <- prod(dim(predict_output))/prod(newdata_dim[1:2])
				
				# Mixed class data frame:
				factor_columns <- sapply(predict_output,function(x) class(x)[1])=="factor"
				
				if(sum(factor_columns)>0) {
#					browser()
					predict_output[,factor_columns] <- as.numeric(predict_output[,factor_columns])
				}
#				if("factor" %in% class(predict_output))
#				{
#					predict_output <- as.numeric(predict_output)
#				}
				
				
#				print(dim(predict_output))
#				print(dim(newdata_complete))
#				if(is.null(dim(newdata_complete))) browser()
				if(!is.null(newdata_complete))
				{
					if(!is.null(dim(predict_output)))
					{
						if(length(dim(predict_output))>1)
						{
							predict_output[!newdata_complete,] <- NA
						} else
						{
							predict_output[!newdata_complete] <- NA
						}
					} else
					{
						predict_output[!newdata_complete] <- NA
					}
					
				}
				
				predict_output_array <- as.matrix(predict_output)
				dim(predict_output_array) <- c(newdata_dim[1:2],nbands_output)
				
				return(gain*predict_output_array+offset)
			}
			
			# Check for factor layers:
			factor_layers <- is.factor(newdata)
			
			additional_args <- list(...)
			additional_args <- c(list(object=object,na.rm.mode=na.rm.mode,gain=gain,offset=offset,ncores=ncores),
					unlist(additional_args,recursive=FALSE))
			additional_args$newdata <- NULL
			
			output <- rasterEngine(newdata=newdata,fun=predict.rasterEngine_function,
					args=additional_args,.packages=(.packages()),filename=filename,debugmode=debugmode,chunk_format="data.frame.dims",verbose=verbose)
			
			return(output)
		}
	}
	return(predict(object,...))
}