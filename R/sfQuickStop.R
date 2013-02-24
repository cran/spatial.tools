#' Quickly stops a parallel snowfall cluster and deregisters it from foreach.
#' 
#' @param ... parameters to pass to sfStop()
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[snowfall]{sfInit}}
#' @details (Even more) quickly stop a snowfall cluster and sets foreach back
#' to sequential mode via registerDoSEQ().
#' @examples
#' sfQuickInit(cpus=2)
#' sfQuickStop()
#' @export

sfQuickStop <- function(...)
{
	cl <- parallel:::defaultCluster()
	registerDoSEQ()
	stopCluster(cl)
}


