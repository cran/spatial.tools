#' Forces a list of Raster*s to all have the same extent.
#' @param extent_reference Raster*. A Raster* object that will provide the extent to all the other Raster*s.  If unassigned, will assume it is the first Raster* in the broken_extents list. 
#' @param broken_extents list of Raster* objects. Raster* objects that will be coerced to the extent_reference's extent.
#' @author Jonathan A. Greenberg (\email{spatial.tools@@estarcion.net})
#' @seealso \code{\link{extent}},\code{\link{stack}}
#' @export


fix_extent <- function(extent_reference,broken_extents)
{
	if(missing(extent_reference))
	{
		extent_reference = broken_extents[[1]]
	}
	
	base_extent=extent(extent_reference)
	fixed_extents=mapply(
		function(x,base_extent) 
		{ 
			extent(x)=base_extent
			return(x)
		},broken_extents,MoreArgs=list(base_extent=base_extent) 
	)
	return(fixed_extents)
}