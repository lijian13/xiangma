
writeFollowers <- function(wxobj, updatestatus = FALSE) {	
	
	tryCatch({
				CONN <- .createConn()
				tbl.old <- dbReadTable(CONN, "followers")
				if (nrow(tbl.old) > 0) Encoding(tbl.old$nickname) <- "UTF-8"
				
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
				} else {
					tbl.new <- data.frame()
				}
				cat(paste0(length(user.add), " follows have been added!\n"))
				
				if (identical(updatestatus, TRUE)) {
					tbl.status <- dbReadTable(CONN, "member_log")
					Encoding(tbl.status$publicname) <- "UTF-8"
					tbl.web0 <- getUsers(wxobj, user.new)
					tbl.web <- tbl.web0[grepl("101", tbl.web0$tagid), ]
					member.add <- setdiff(tbl.web$openid, tbl.status$openid[tbl.status$status == 1])
					member.rm <- setdiff(tbl.status$openid[tbl.status$status == 1], tbl.web$openid)				
					tmp.namechange <- merge(unique(tbl.status[, c("openid", "publicname")]), tbl.web0[, c("openid", "remark")], all.x = TRUE)
					tmp.namechange <- tmp.namechange[!is.na(tmp.namechange$remark),]
					member.change.df <- tmp.namechange[tmp.namechange$publicname != tmp.namechange$remark, ]
					if (length(member.add) > 0) {
						tmp.add <- tbl.web[tbl.web$openid %in% member.add, ]
						out.add <- data.frame(openid = tmp.add$openid, publicname = tmp.add$remark, 
								jointime = tmp.add$subscribe_time, leavetime = NA, status = 1)
						dbWriteTable(CONN, "member_log", out.add, row.names = FALSE, append = TRUE)
						cat(paste0(length(member.add), " group members have been added!\n"))
					}
					if (length(member.rm) > 0) {
						for (i in 1:length(member.rm)) {
							strsql <- paste0("update member_log set leavetime = '", as.character(Sys.time()), 
									"', status = 0 where openid = '", member.rm[i], "' and status = 1")
							rs <- dbSendQuery(CONN, strsql)	
							dbClearResult(rs)							
						}
						cat(paste0(length(member.rm), " group members have been removed!\n"))
					}
					if (nrow(member.change.df) > 0) {
						for (i in 1:nrow(member.change.df)) {
							strsql <- paste0("update member_log set publicname = '", member.change.df[i, "remark"], 
									"' where openid = '", member.change.df[i, "openid"], "'")
							rs <- dbSendQuery(CONN, strsql)	
							dbClearResult(rs)							
						}
						cat(paste0(nrow(member.change.df), " member names have been changed!\n"))
					}
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
