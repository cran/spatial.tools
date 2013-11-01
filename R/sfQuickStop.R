#' Quickly stops a parallel snowfall cluster and deregisters it from foreach.
#' 
#' @param ... parameters to pass to sfStop()
#' @author Jonathan A. Greenberg
#' @details (Even more) quickly stop a snowfall cluster and sets foreach back
#' to sequential mode via registerDoSEQ().
#' @examples
#' sfQuickInit(cpus=2)
#' sfQuickStop()
#' @import parallel
#' @import foreach
#' @export

sfQuickStop <- function(...)
{
	cl <- getOption("spatial.tools.current.cl")
	registerDoSEQ()
	stopCluster(cl)
	options(spatial.tools.current.cl=NULL)
}


