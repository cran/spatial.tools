#' Quickly initializes a parallel snowfall cluster and registers it with foreach.
#' 
#' @param cpus Number of cpus.  Will default to the max available cpus.
#' @param ... parameters to pass to sfInit()
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[snowfall]{sfInit}}
#' @details (Even more) quickly start a snowfall cluster with maximum available
#' cpus, parallel = TRUE, and type = "SOCK" and registers it with foreach.  
#' @examples 
#' sfQuickInit(cpus=2)
#' sfQuickStop()
#' @export

sfQuickInit <- function(cpus,...)
{

	if(missing("cpus"))
	{
		cpus <- parallel::detectCores()
	}
	
	cl <- makeCluster(spec=cpus,type="PSOCK")
	setDefaultCluster(cl=cl)
	registerDoParallel(cl)
	
#	sfInit(cpus=cpus,parallel=TRUE,...)
#	if(any(search()=="package:foreach"))
#	{
#		require("doSNOW")
#		cl <- sfGetCluster()
#		registerDoSNOW(cl)
#	}
	return(cl)
}


