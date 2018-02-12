


.createDBConfig <- function(host = "localhost", port = "5432", dbname = "xiangma", user = "", password = "") {
	connpath <- file.path(Sys.getenv("APPDATA"), "xiangma", "db")
	if (!file.exists(connpath)) dir.create(connpath, recursive = TRUE)
	objlist <- list(host = host, port = port, dbname = dbname, user = user, password = password)	
	objfile <- file(file.path(connpath, "config.json") , open = "w" )
	writeLines(toJSON(objlist), objfile)
	close(objfile)
	invisible(TRUE)
}


.createConn <- function() {
	connpath <- file.path(Sys.getenv("APPDATA"), "xiangma", "db")
	if (!file.exists(file.path(connpath, "config.json"))) {
		stop("Please use '.createDBConfig' to set the config file!")
	}
	conlist <- fromJSON(file.path(connpath, "config.json"))
	CONN <- dbConnect(dbDriver("PostgreSQL"), user = conlist$user, password = conlist$password, dbname = conlist$dbname, host = conlist$host, port = conlist$port)
	return(CONN)
}

.similarity <- function(s1, s2) {
	wdf1 <- createWordFreq(segmentCN(s1))
	wdf2 <- createWordFreq(segmentCN(s2))
	names(wdf2)[2] <- "freq2"
	wdf <- merge(wdf1, wdf2, all.x = TRUE, all.y = TRUE)
	wdf$freq[is.na(wdf$freq)] <- 0
	wdf$freq2[is.na(wdf$freq2)] <- 0
	OUT <- crossprod(wdf$freq, wdf$freq2)[1,1] / (sum(wdf$freq^2)^0.5 * sum(wdf$freq2^2)^0.5)
	return(OUT)
}

.dealwithTags <- function(tagv, maxn = 3, maxc = 5) {
	s1 <- unlist(strsplit(tagv, split = ";"))
	s2 <- s1[!grepl(".*,.*,", s1)]
	
	OUT <- as.data.frame(do.call("rbind", strsplit(s2, split = ",")), stringsAsFactors = FALSE)
	names(OUT) <- c("tag", "freq")
	OUT <- OUT[nchar(OUT$tag) <= maxc, ]

	OUT$freq <- log(as.numeric(OUT$freq))
	OUT <- summarise(group_by(OUT, tag), freq = sum(freq))
	OUT <- arrange(OUT, desc(freq))
	return(OUT[1:min(maxn, nrow(OUT)), ])
}
