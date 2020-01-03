
updateDoubanID <- function(month = substr(Sys.time(), 1, 7), sleepmean = 5) {	
	tryCatch({
				OUT <- list()
				CONN <- .createConn()
				commdf <- dbGetQuery(CONN, paste0("SELECT title, msgid from comment_log where time like '", month, "%' and doubanid is null"))
				bookdist <- dbGetQuery(CONN, "SELECT distinct id from douban_list")[[1]]
				Encoding(commdf$title) <- "UTF-8" 
				for (i in 1:nrow(commdf)) {
					tmp.dbk <- dbGetQuery(CONN, paste0("SELECT id, title from douban_list where title = '", commdf$title[i], "'"))
					if (nrow(tmp.dbk) > 0) {
						tmp.search <- tmp.dbk
						tmp.search$title <- .iconvutf8(tmp.search$title)
					} else {
						tmp.dbweb <- searchDoubanList(commdf$title[i])
						tmp.dbweb <- tmp.dbweb[grepl("\\[\u4E66\u7C4D\\]", tmp.dbweb$title), ]
						tmp.dbweb$title <- strstrip(gsub("\\[\u4E66\u7C4D\\]", "", tmp.dbweb$title))
						tmp.dbweb$title <- sapply(strsplit(tmp.dbweb$title, "\\s+"), "[", 1)
						tmp.dbk <- dbGetQuery(CONN, paste0("SELECT id, title from douban_list where title = '",tmp.dbweb$title[1], "'"))
						if (nrow(tmp.dbk) > 0) {
							tmp.search <- tmp.dbk
							tmp.search$title <- .iconvutf8(tmp.search$title)
						} else {
							tmp.search <- tmp.dbweb[1, c("id", "title")]
						}	
					}					
					OUT[[i]] <- try(fixDoubanID(msgid = commdf$msgid[i], doubanid = tmp.search$id[1]), silent = TRUE)
					if (inherits(OUT[[i]], "data.frame")) {
						cat("[", i, "]", paste0(commdf$msgid[i], ": ", commdf$title[i], "\n"))
					} else {
						cat("[", i, "]", paste0(commdf$msgid[i], ": Error\n"))
					}
					Sys.sleep(round(abs(rnorm(1, sleepmean, sleepmean/4)), 0))
				}
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
				return(OUT)
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
				}  else {
					tmp.douban <- tmp.exists
				}	
				if (grepl("^LW", doubanid)) {
					strinclude = ", include = 0"
				} else {
					strinclude = ", include = 1"
				}
				strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", gsub("'", "''", tmp.douban$title[1]), "'", strinclude, " where msgid = '", msgid, "'")
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
				if (nrow(tmp.exists) > 0) {
					stop("this id has been existed!")
				} 
				tmp.douban <- data.frame(id = doubanid, title = toUTF8(title), 
						subtitle = toUTF8(subtitle), author = toUTF8(author), 
						translator = toUTF8(translator), pubdate = toUTF8(pubdate), 
						publisher = toUTF8(publisher), stringsAsFactors = FALSE)
				dbWriteTable(CONN, "douban_list", tmp.douban, row.names = FALSE, append = TRUE)
				if (grepl("^LW", doubanid)) {
					strinclude = ", include = 0"
				} else {
					strinclude = ", include = 1"
				}
				strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", tmp.douban$title[1], "'", strinclude, " where msgid = '", msgid, "'")
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


# api (old)
updateDoubanID.api <- function(month = substr(Sys.time(), 1, 7)) {	
	tryCatch({
				CONN <- .createConn()
				commdf <- dbGetQuery(CONN, paste0("SELECT title, msgid from comment_log where time like '", month, "%' and doubanid is null"))
				bookdist <- dbGetQuery(CONN, "SELECT distinct id from douban_list")[[1]]
				Encoding(commdf$title) <- "UTF-8" 
				for (i in 1:nrow(commdf)) {
					tmp.douban <- suppressWarnings(searchDouban(commdf$title[i], detail = TRUE))
					#tmp.douban[which.max(tmp.douban$ratingnum), ]
					if (nrow(tmp.douban) > 0) {
						strsql <- paste0("update comment_log set doubanid = '", tmp.douban$id[1], "', doubantitle = '", tmp.douban$title[1], "', include = 1 where msgid = '", commdf$msgid[i], "'")
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