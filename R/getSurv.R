
getSurv <- function() {
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember")
				tbl.book <- dbGetQuery(CONN, "SELECT openid, max(time) as maxtime1, min(time) as mintime1 from comment_log group by openid")
				tbl.chat <- dbGetQuery(CONN, "SELECT openid, max(createtime) as maxtime2, min(createtime) as mintime2 from chatbot2017_log group by openid")
				Encoding(tbl.user$publicname) <- "UTF-8"
				survdf <- tbl.user[, c("openid", "publicname", "status", "jointime", "leavetime")]
				survdf <- merge(survdf, tbl.book, all.x = TRUE)
				survdf <- merge(survdf, tbl.chat, all.x = TRUE)		
				survdf[survdf$status == 1, "leavetime"] <- as.character(Sys.time())
				
				for (i in 1:nrow(survdf)) {
					if (is.na(survdf$jointime[i])) {
						survdf$jointime[i] <- min(c(survdf$mintime1[i], survdf$mintime2[i]), na.rm = TRUE)
					}
					if (is.na(survdf$leavetime[i])) {
						survdf$leavetime[i] <- max(c(survdf$maxtime1[i], survdf$maxtime2[i]), na.rm = TRUE)
					}
				}
				
				OUT <- survdf[, c("openid", "publicname", "status", "jointime", "leavetime")]
				OUT <- OUT[!is.na(OUT$jointime)&!is.na(OUT$leavetime), ]
				OUT$time <- as.numeric(difftime(strptime(OUT$leavetime, format = "%Y-%m-%d %H:%M:%S"), strptime(OUT$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days"))
				OUT <- OUT[OUT$time > 31, ]
				rownames(OUT) <- 1:nrow(OUT)
				OUT <- OUT[order(OUT$time, decreasing = TRUE), ]
				OUT$status <- 2 - OUT$status
				return(OUT)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
