#' Easier-to-use function for grabbing a block of data out of a Raster*.
#' 
#' @param x Raster* Some input Raster* object.
#' @param r1 Numeric. The start row of the chunk.
#' @param r2 Numeric. The end row of the chunk.
#' @param c1 Numeric. The start column of the chunk.
#' @param c2 Numeric The end row of the chunk.
#' @param format Character. If "array" (default), the chunk will be returned in a 3-d array with dimensions representing column,row,and layer.  If "raster", the chunk will be returned as a Raster* object.
#' @param ... Other parameters.
#' 
#' @return An array or raster object.
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[raster]{getValues}}
#' @examples
#' tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="spatial.tools"))
#' mychunk <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50)
#' class(mychunk)
#' dim(mychunk)
#' mychunk_raster <- getValuesBlock_enhanced(tahoe_highrez,r1=100,r2=110,c1=20,c2=50,format="raster")
#' mychunk_raster
#' @export

getValuesBlock_enhanced=function(x,r1=1,r2=nrow(x),c1=1,c2=ncol(x),format="array",...)
{	
	if(format=="array")
	{
		layer_names=names(x)
		
		getvalues_raw=as.numeric(getValues(crop(x, extent(x, r1=r1, r2=r2, c1=c1,c2=c2))))
		getvalues_raw_nrows=r2-r1+1
		getvalues_raw_ncols=c2-c1+1
		getvalues_raw_nlayers=nlayers(x)
		
		# Test the input file.
		if(getvalues_raw_nlayers==1)
		{
		# Raster
			getvalues_array=array(data=getvalues_raw,
				dim=c(getvalues_raw_ncols,getvalues_raw_nrows,getvalues_raw_nlayers))		
		} else
		{
		# Brick or stack
			getvalues_array=array(data=getvalues_raw,
				dim=c(getvalues_raw_ncols,getvalues_raw_nrows,getvalues_raw_nlayers))
		}
		dimnames(getvalues_array) <- list(NULL,NULL,NULL)
		if(!is.null(layer_names)) dimnames(getvalues_array)[[3]]=layer_names
		return(getvalues_array)
	}
	if(format=="raster")
	{
		return(crop(x, extent(x, r1=r1, r2=r2, c1=c1,c2=c2)))
	}
}