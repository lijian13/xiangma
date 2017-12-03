
getWeek <- function(strtime, format = "%Y-%m-%d %H:%M:%S") {
	if (!inherits(strtime, c("POSIXt", "Date"))) {
		strtime <- strptime(strtime, format = format)
	}
	OUT <- as.numeric(as.Date(strtime) - as.Date("2015-11-23")) %/% 7 + 1
	return(OUT)
}
