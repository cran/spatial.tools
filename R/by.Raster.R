

by.Raster <- function(x,INDICES,FUN,verbose=FALSE,...)
{
	by_focal_hpc_function <- function(x,INDICES,FUN,...)
	{
		by_focal_output <- apply(X=x,MARGIN=c(1,2),
			FUN=function(X,INDICES,fun)
			{
				by_output <- by(X,INDICES,fun)
			},INDICES=INDICES,fun=FUN)
	
		by_focal_output <- aperm(by_focal_output,c(2,3,1))
		return(by_focal_output)
	}
	
	by.Raster_output <- focal_hpc(x,fun=by_focal_hpc_function,args=list(INDICES=INDICES,FUN=FUN),verbose=verbose)
}