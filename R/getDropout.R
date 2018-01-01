
getDropout <- function(month = substr(Sys.time(), 1, 7)) {
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")
				Encoding(tbl.user$publicname) <- "UTF-8"
				OUT <- tbl.user[grep(month, tbl.user$leavetime), ]
				rownames(OUT) <- NULL
				
				return(OUT[order(OUT$leavetime), c("publicname", "jointime", "leavetime")])
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
