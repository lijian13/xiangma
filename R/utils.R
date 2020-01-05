

.iconvutf8 <- function(X) {
	iconv(X, "UTF-8", "UTF-8")
}

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
	wdf1 <- createWordFreq(segmentCN(s1), onlyCN = FALSE)
	if (length(s2) == 1) {
		wdf2 <- list(createWordFreq(segmentCN(s2), onlyCN = FALSE))
	} else {
		wdf2 <- lapply(segmentCN(s2), createWordFreq,  onlyCN = FALSE)
	}
	wdf <- lapply(wdf2, FUN = function(X) {names(X)[2] <- "freq2"; merge(wdf1, X, all.x = TRUE, all.y = TRUE)})
	wdf <- lapply(wdf, FUN = function(X) {X$freq[is.na(X$freq)] <- 0; X$freq2[is.na(X$freq2)] <- 0;X})
	OUT <- max(sapply(wdf, FUN = function(X) crossprod(X$freq, X$freq2)[1,1] / (sum(X$freq^2)^0.5 * sum(X$freq2^2)^0.5)))
	return(OUT)
}

.dealwithTags <- function(tagv, maxn = 4, maxc = 5, maxlabel = 5) {
	
	a1 <- strsplit(tagv, split = ";")
	a2 <- lapply(a1, FUN = function(X) X[!grepl(".*,.*,", X)])
	a3 <- a2[sapply(a2, length) > 0]
	a4 <- lapply(a3, FUN = function(X) as.data.frame(do.call("rbind", strsplit(X, split = ",")), stringsAsFactors = FALSE))
	a5 <- lapply(a4, FUN = function(X) {names(X) <- c("tag", "freq"); X$freq <- as.numeric(X$freq); X$freq <- X$freq / max(X$freq); return(X)})
	a6 <- lapply(a5, FUN = function(X) X[1:min(5, nrow(X)),])
	
	OUT <- do.call("rbind", a6)
	OUT <- OUT[nchar(OUT$tag) <= maxc, ]
	OUT$tag <- toTrad(OUT$tag, rev = TRUE)
	OUT$tag[OUT$tag == "\u79D1\u5E7B\u5C0F\u8BF4"] <- "\u79D1\u5E7B"
	OUT$tag[OUT$tag == "\u5FC3\u7406"] <- "\u5FC3\u7406\u5B66"
	OUT$freq <- (OUT$freq)^0.5
	OUT <- summarise(group_by(OUT, tag), freq = sum(freq))
	OUT <- arrange(OUT, desc(freq))
	
	#s1 <- unlist(strsplit(tagv, split = ";"))
	#s2 <- s1[!grepl(".*,.*,", s1)]
	
	#OUT <- as.data.frame(do.call("rbind", strsplit(s2, split = ",")), stringsAsFactors = FALSE)
	#names(OUT) <- c("tag", "freq")
	#OUT <- OUT[nchar(OUT$tag) <= maxc, ]

	#OUT$freq <- log(as.numeric(OUT$freq))
	#OUT <- summarise(group_by(OUT, tag), freq = sum(freq))
	#OUT <- arrange(OUT, desc(freq))
	return(OUT[1:min(maxn, nrow(OUT)), ])
}

.dealwithTagsTM <- function(tagv) {
	s1 <- unlist(strsplit(tagv, split = ";"))
	s2 <- s1[!grepl(".*,.*,", s1)]
	
	OUT <- as.data.frame(do.call("rbind", strsplit(s2, split = ",")), stringsAsFactors = FALSE)
	names(OUT) <- c("tag", "hot")
	OUT$tag <- gsub("^\\s+", "", OUT$tag)
	
	return(paste0(OUT$tag, collapse = " "))
}

.verifyUsers <- function(userdf, starttime = "2015-01-01 00:00:00", endtime = "2076-01-01 00:00:00", startvar = "jointime", endvar = "leavetime") {
	ut1 <- strptime(userdf[[startvar]], format = "%Y-%m-%d %H:%M:%S")
	ut2 <- strptime(userdf[[endvar]], format = "%Y-%m-%d %H:%M:%S")
	ut2[is.na(ut2)] <- strptime("2075-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
	if (inherits(starttime, "POSIXlt")) {
		vt1 <- starttime
	} else {
		vt1 <- strptime(starttime, format = "%Y-%m-%d %H:%M:%S")
	}
	if (inherits(endtime, "POSIXlt")) {
		vt2 <- endtime
	} else {
		vt2 <- strptime(endtime, format = "%Y-%m-%d %H:%M:%S")
	}
	OUT <- userdf
	OUT$verify <- 1
	OUT$verify[vt1 > ut2 | vt2 < ut1] <- 0
	return(OUT)
}

.cleanAuthors <- function(autv) {
	OUT <- sapply(strsplit(autv, split = ","), "[", 1)
	OUT <- gsub("\\[.*?\\]", "", OUT)
	OUT <- gsub("\uFF08.*?\uFF09", "", OUT)
	OUT <- gsub("\uFF3B.*?\uFF3D", "", OUT)
	OUT <- gsub("\\(.*?\\)", "", OUT)
	OUT <- gsub("\u3014.*?\u3015", "", OUT)
	OUT <- gsub("\u3010.*?\u3011", "", OUT)
	OUT <- gsub("\\s+", "", OUT)
	OUT[is.na(OUT)] <- ""
	return(OUT)
}
