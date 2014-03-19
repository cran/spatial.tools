dataType_converter <- function(from,fromFormat="raster",toFormat="mmap")
{
	dataType_table <- list(
			raster=c(
					"LOG1S",
					"INT1S",
					"INT1U",
					"INT2S",
					"INT2U",
					"INT4S",
					"INT4U",
					"FLT4S",
					"FLT8S",
					NA,
					NA,
					NA,
					NA,
					NA,
					NA),
			mmap=c(
					logi8,
					int8,
					uint8,
					int16,
					uint16,
					int32,
					NA,
					real32,
					real64,
					uchar,
					logi32,
					int24,
					uint24,
					int64,
					cplx
					)
	)
	
	if(fromFormat=="raster")
	{
		id <- (dataType_table$raster==from) & (!is.na(dataType_table$raster))
	}
	
	if(toFormat=="mmap")
	{
		return(dataType_table$mmap[id][[1]])
	}
}