

insertReadRecord <- function(user, book, text, currtime = as.character(Sys.time())) {	
	tryCatch({
				CONN <- .createConn()
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")	
				tbl.user$publicname <- toUTF8(tbl.user$publicname)
				user <- toUTF8(user)
				book <- toUTF8(book)
				text <- toUTF8(text)
				openid <- tbl.user$openid[tbl.user$publicname == user]
				
				week <- getWeek(currtime)
				msgid <- paste0("manual", gsub("[^0-9]", "", currtime))
				dbdf <- data.frame(openid = openid,
						title = book,
						week = week,
						time = currtime,
						content = text,
						msgid = msgid,
						stringsAsFactors = FALSE)
				dbWriteTable(CONN, "comment_log", dbdf, row.names = FALSE, append = TRUE)
				invisible(TRUE)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
