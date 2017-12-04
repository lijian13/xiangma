
writeFollowers <- function(wxobj) {	
	
	tryCatch({
				CONN <- .createConn()
				tbl.old <- dbReadTable(CONN, "followers")
				
				user.old <- tbl.old$openid[tbl.old$subscribe == 1]
				user.new <- getUserList(wxobj)
				user.add <- setdiff(user.new, user.old)
				user.rm <- setdiff(user.old, user.new)
				
				if (length(user.rm) > 0) {
					strsql <- paste0("update followers set subscribe = 0 where openid in (", paste0("'", user.rm, "'", collapse = " ,"), ")")
					dbSendQuery(CONN, strsql)
				}
				if (length(user.add) > 0) {
					tbl.new <- getUsers(wxobj, user.add)
					tbl.new <- tbl.new[order(tbl.new$subscribe_time), ] 
					#tbl.new$nickname <- gsub("[^[[:ascii:]\u4e00-\u9fa5]", "", tbl.new$nickname, perl = TRUE)
					dbWriteTable(CONN, "followers", tbl.new, row.names = FALSE, append = TRUE)
				}
				cat(paste0(length(user.add), " rows have been added!\n"))
				invisible(TRUE)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
	
}
