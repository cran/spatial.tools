#' Find GDAL installation
#' 
#' Tools for properly locating and determing the state of the Geospatial Data Abstraction Library
#' 
#' @param return_drivers Logical. Return a table of GDAL drivers? Default is TRUE. 
#' @param return_python_utilities Logical. Return a vector of available python utilities? Default is TRUE. 
#' @param return_most_current Logical. Return only the most current version of GDAL (if multiple installs are present)? Default is TRUE.
#' @param required_drivers Character. What driver is required?  Default is no required drivers.
#' @param verbose logical. Enable verbose execution? Default is FALSE.  
#' @return A list with one element per GDAL installation.  See Description for parameter names.
#' @author Jonathan A. Greenberg
#' @keywords format
#' @details get_gdal_installation is designed to help determine the correct path to the
#' Geospatial Data Abstraction Library, even if the PATH is not set on the local computer.  It
#' accomplishes this by brute-force searching for the gdalinfo(.exe) executable on the user's
#' local system, starting at the root level ("/" on Unix-alikes, "c:" on Windows).  It can
#' also (optionally) return information on the available drivers, python utilities, and can
#' determine the "best" (most current version) of GDAL if there are multiple installs (more
#' common on Windows boxes than Unix-alikes).
#' 
#' This code is a heavily modified version of code found in the MODIS package.
#' 
#' @examples \dontrun{ 
#' # Determine the most current GDAL installations:
#' mygdals <- get_gdal_installation()
#' mygdals[[1]]$gdal_path
#' mygdals[[1]]$version
#' mygdals[[1]]$drivers
#' mygdals[[1]]$python_utilities
#' # Determine all available GDAL installations:
#' mygdals <- get_gdal_installation(return_most_current=FALSE)
#' sapply(mygdals,function(X) X$gdal_path)
#' # Only return GDAL installs that support a given driver: 
#' mygdals <- get_gdal_installation(required_drivers="HDF")
#' }
#' @export

get_gdal_installation=function(return_drivers=TRUE,
	return_python_utilities=TRUE,return_most_current=TRUE,required_drivers=NULL,
	verbose=FALSE)
{
	
	# First search for paths:
	if (.Platform$OS=="unix")
	{    
		# Unix-likes
		gdal <- try(system("gdalinfo --version",intern=TRUE),silent=TRUE)
		if (inherits(gdal,"try-error"))
		{
			if(verbose) message(cat("GDAL not found in PATH, trying a search... This could take some time...\n"))
			gdalinfo_paths <- system("find / -name gdalinfo",intern=TRUE)
			if(length(gdalinfo_paths)==0)
			{
				if(verbose) message(paste("No GDAL was found."))
				return(NULL)
			} else
			{
				gdal_installation_list=vector(mode="list",length=length(gdalinfo_paths))
			}
		} else
		{
			gdalinfo_paths <- system("which gdalinfo")
			gdal_installation_list=vector(mode="list",length=1)
		}
	} else 
	{	
		# Windows
		cmd <- 'gdalinfo --version'
		gdalinfo_paths=list.files(path="c:/",pattern="^gdalinfo.exe$", full.names=TRUE, recursive=TRUE,include.dirs=TRUE)
		if(length(gdalinfo_paths)==0) 
		{
			if(verbose) message(paste("No GDAL was found."))
			return(NULL)
		} else
		{
			gdal_installation_list=vector(mode="list",length=length(gdalinfo_paths))
		}
	}
#	if(verbose) message("Found everything...")
	
	
# Determine the versions:
	for(i in 1:length(gdalinfo_paths))
	{
		cmd <- paste("'",gdalinfo_paths[i],"'"," --version",sep="")
#		setwd(gdalinfo_paths[i])
		gdal_installation_list[[i]]$gdal_path=dirname(gdalinfo_paths[i])
# Does shell work here?
		if (.Platform$OS=="unix") 
		{
			gdal_version <- system(cmd,intern=TRUE) 
		} else 
		{
			gdal_version <- shell(cmd,intern=TRUE)
		}
		
		if(length(grep(glob2rx("GDAL*"),gdal_version)) != 0)
		{
			
			version=strsplit(strsplit(gdal_version,",")[[1]][1]," ")[[1]][2]
			gdal_installation_list[[i]]$version <- version
		} else
		{
			# Broken install
			if(verbose)
			{
				message(paste("Probably broken install of gdal at ",gdal_installation_list[[i]]$gdal_path),sep="")
				gdal_installation_list[[i]]$version=NA
			}
		}
	}
	
	
	if(return_drivers)
	{
		for(i in 1:length(gdalinfo_paths))
		{
			drivers_cmd <- paste("'",gdalinfo_paths[i],"'"," --formats",sep="")
			if (.Platform$OS=="unix") 
			{
				drivers_raw <- system(drivers_cmd,intern=TRUE) 
			} else 
			{
				drivers_raw <- shell(drivers_cmd,intern=TRUE)
			}
			drivers=strsplit(drivers_raw,":")
			driver_names=gsub("^ ","",sapply(drivers,function(x) { x[2] })) # Need to remove spaces
			driver_codes_perm=strsplit(sapply(drivers,function(x) { x[1] }),"\\(")
			driver_codes=gsub(" ","",sapply(driver_codes_perm,function(x) { x[1] }),fixed=TRUE)
			driver_perm=gsub("\\)","",sapply(driver_codes_perm,function(x) { x[2] }))
			drivers_dataframe=data.frame(format_code=driver_codes,format_rw=driver_perm,format_name=driver_names)
			
			drivers_dataframe=drivers_dataframe[2:dim(drivers_dataframe)[1],]
			gdal_installation_list[[i]]$drivers=drivers_dataframe
		}
	}
	
	if(return_python_utilities)
	{
		for(i in 1:length(gdalinfo_paths))
		{
			gdal_installation_list[[i]]$python_utilities <- dir(gdal_installation_list[[i]]$gdal_path,pattern=".py") 
		}
	}
	
	
	if(!missing(required_drivers))
	{
		format_checked=vector(mode="logical",length=length(gdal_installation_list))
		check_for_drivers_n=length(required_drivers)
		for(i in 1:length(gdal_installation_list))
		{
			check=required_drivers %in% gdal_installation_list[[i]]$drivers$format_code
			format_checked[i]=sum(check)==check_for_drivers_n
		}
		if(sum(format_checked)==0)
		{
			if(verbose) message("No GDAL installations match those drivers...")
			return(NULL)
		} else
		{
			gdal_installation_list=gdal_installation_list[format_checked]
		}
	}
	
	if(return_most_current)
	{
		if(length(gdal_installation_list)>1)
		{
			versions <- sapply(gdal_installation_list,function(X) X$version)
			best_version <- (order(versions,decreasing=TRUE)==1)
			gdal_installation_list <- gdal_installation_list[best_version]
		}
		
	}
	
	return(gdal_installation_list)
}