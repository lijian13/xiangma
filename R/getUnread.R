
getUnread <- function(month = substr(Sys.time(), 1, 7)) {	
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember where status = 1")
				tbl.book <- dbGetQuery(CONN, paste0("SELECT * from comment_log where time >= '", paste0(month, "-01 00:00:00"), "'"))
				tbl.lastdate <- dbGetQuery(CONN, "SELECT openid, max(time) as lastreadtime, count(openid) as total from comment_log group by openid")
				
				Encoding(tbl.user$groupname) <- "UTF-8"
				Encoding(tbl.user$publicname) <- "UTF-8"
				
				if (nrow(tbl.book) == 0) {
					tbl.book <- data.frame(openid = character(), stringsAsFactors = FALSE)
				}
				
				OUT <- tbl.user[!tbl.user$openid %in% tbl.book$openid, ]
				if (length(which(!is.na(OUT$jointime) & OUT$jointime >= paste0(month, "-01 00:00:00"))) > 0) {
					OUT <- OUT[-which(!is.na(OUT$jointime) & OUT$jointime >= paste0(month, "-01 00:00:00")), ]
				}
				
				if (nrow(OUT) > 0) {
					OUT <- merge(OUT[, c("openid", "publicname")], tbl.lastdate, all.x = TRUE)
					OUT <- OUT[order(OUT$lastreadtime, decreasing = TRUE), ]
					OUT$openid <- NULL
					rownames(OUT) <- 1:nrow(OUT)
				}
				return(OUT)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
