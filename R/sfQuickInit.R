#' Quickly initializes a parallel cluster and registers it with foreach.
#' 
#' @param cpus Number of cpus.  Will default to the max available cpus.
#' @param methods Logical. Load the methods package? (if FALSE, faster startup). Default=FALSE.
#' @param ... parameters to pass to sfInit()
#' @author Jonathan A. Greenberg
#' @details (Even more) quickly start a parallel cluster with maximum available
#' cpus, parallel = TRUE, and type = "SOCK" and registers it with foreach.  
#' @examples 
#' sfQuickInit(cpus=2)
#' sfQuickStop()
#' @export

sfQuickInit <- function(cpus,methods=FALSE,...)
{
	if(missing("cpus"))
	{
		cpus <- detectCores()
	}
	
	cl <- makeCluster(spec=cpus,type="PSOCK",methods=methods)
	setDefaultCluster(cl=cl)
	registerDoParallel(cl)
	return(cl)
}


