
updateNewOpenID <- function() {	
	tryCatch({
				CONN <- .createConn()
				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember")
				tbl.user <- tbl.user[tbl.user$groupid != tbl.user$openid, ]
				vec.open1 <- dbGetQuery(CONN, "SELECT distinct openid from comment_log")[[1]]
				vec.open2 <- dbGetQuery(CONN, "SELECT distinct openid from chatbot2017_log")[[1]]
				vec.open <- union(vec.open1[nchar(vec.open1) > 6],  vec.open2[nchar(vec.open2) > 6])
				tbl.user <- tbl.user[!tbl.user$openid %in% vec.open, ]
				for (i in 1:nrow(tbl.user)) {
					strsql <- paste0("update comment_log set openid = '", tbl.user$openid[i], "' where openid = '", tbl.user$groupid[i], "'")
					rs <- dbSendQuery(CONN, strsql)	
					dbClearResult(rs)
					strsql <- paste0("update chatbot2017_log set openid = '", tbl.user$openid[i], "' where openid = '", tbl.user$groupid[i], "'")
					rs <- dbSendQuery(CONN, strsql)	
					dbClearResult(rs)
				}
				invisible(TRUE)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
