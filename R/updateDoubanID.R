
updateDoubanID <- function(month = substr(Sys.time(), 1, 7)) {	
	tryCatch({
				CONN <- .createConn()
				commdf <- dbGetQuery(CONN, paste0("SELECT title, msgid from comment_log where time like '", month, "%' and doubanid is null"))
				bookdist <- dbGetQuery(CONN, "SELECT distinct id from douban_list")[[1]]
				Encoding(commdf$title) <- "UTF-8" 
				for (i in 1:nrow(commdf)) {
					tmp.douban <- suppressWarnings(searchDouban(commdf$title[i], detail = TRUE))
					#tmp.douban[which.max(tmp.douban$ratingnum), ]
					if (nrow(tmp.douban) > 0) {
						strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", tmp.douban$title[1], "' where msgid = '", commdf$msgid[i], "'")
						rs <- dbSendQuery(CONN, strsql)	
						dbClearResult(rs)
						if (!tmp.douban$id[1] %in% bookdist) {
							dbWriteTable(CONN, "douban_list", tmp.douban[1, ], row.names = FALSE, append = TRUE)
						}
					}
				}
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
				return(commdf)
			}
	)
}

fixDoubanID <- function(msgid, doubanid) {
	tryCatch({
				CONN <- .createConn()
				tmp.exists <- dbGetQuery(CONN, paste0("select * from douban_list where id = '", doubanid, "'"))
				if (nrow(tmp.exists) == 0) {
					tmp.douban <- suppressWarnings(searchDoubanBook(doubanid, detail = TRUE))
					dbWriteTable(CONN, "douban_list", tmp.douban, row.names = FALSE, append = TRUE)
				} 	
				strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", gsub("'", "''", tmp.douban$title[1]), "' where msgid = '", msgid, "'")
				rs <- dbSendQuery(CONN, strsql)	
				dbClearResult(rs)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
				return(tmp.douban)
			}
	)
}

addDoubanID <- function(msgid, doubanid, title, subtitle = "", author = "", translator = "", pubdate = "", publisher = "") {
	tryCatch({
				CONN <- .createConn()
				tmp.exists <- dbGetQuery(CONN, paste0("select * from douban_list where id = '", doubanid, "'"))
				if (nrow(tmp.exists) == 0) {
					stop("this id has been existed!")
				} 
				tmp.douban <- data.frame(id = doubanid, title = toUTF8(title), 
						subtitle = toUTF8(subtitle), author = toUTF8(author), 
						translator = toUTF8(translator), pubdate = toUTF8(pubdate), 
						publisher = toUTF8(publisher), stringsAsFactors = FALSE)
				dbWriteTable(CONN, "douban_list", tmp.douban, row.names = FALSE, append = TRUE)
				strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", tmp.douban$title[1], "' where msgid = '", msgid, "'")
				rs <- dbSendQuery(CONN, strsql)	
				dbClearResult(rs)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
				return(tmp.douban)
			}
	)
}

