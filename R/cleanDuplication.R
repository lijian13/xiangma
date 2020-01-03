
listDuplication <- function(threshold = 0.8, month = substr(Sys.time(), 1, 7), detail = FALSE) {
	tryCatch({
				CONN <- .createConn()
				commdf <- dbGetQuery(CONN, "select * from comment_log where doubanid in (select doubanid from comment_log group by openid, doubanid having count(openid) > 1)")
				memberdf <- dbGetQuery(CONN, "select openid, min(publicname) as publicname from member_log group by openid")

				d1 <- summarise (group_by(commdf, openid, doubanid), num = length(msgid))
				d2 <- d1[d1$num > 1, ]
				commdf <- merge(commdf, memberdf, all.x = TRUE)
				
				l1 <- lapply(1:nrow(d2), FUN = function(X) commdf[commdf$openid == d2$openid[X] & commdf$doubanid == d2$doubanid[X], c("msgid", "publicname", "doubanid", "time", "title", "doubantitle", "include", "content")])
				l2 <- lapply(l1, FUN = function(X) {X$title <- .iconvutf8(X$title);X$doubantitle <- .iconvutf8(X$doubantitle);X$content <- .iconvutf8(X$content);X$publicname <- .iconvutf8(X$publicname);X})
				if (identical(detail, TRUE)) {
					l3 <- l2
				} else {
					l3 <- l2[sapply(l2, FUN = function(X) sum(X$include %in% c(1, NA)) > 1)]
				}
				if (length(l3) == 0) {
					OUT <- list()
					stop("No duplication!")	
				}
				
				OUT <- list()
				for (i in 1:length(l3)) {
					tmp.out <- l3[[i]][order(l3[[i]]$time, decreasing = TRUE), ]
					tmp.out$include[which(substr(tmp.out$time, 1, 7) == substr(tmp.out$time[1], 1, 7))] <- 0
					tmp.out$include[1] <- 1
					tmp.out <- l3[[i]][order(l3[[i]]$time), ]
					tmp.out$sim <- 1
					tmp.out$include.new <- 1
					for (j in 2:nrow(tmp.out)) {
						tmp.out$sim[j] <- .similarity(tmp.out$content[j], tmp.out$content[1:(j-1)])
						#OUT[[i]]$content <- NULL
						if (tmp.out$sim[j] > threshold) {
							tmp.out$include.new[j] <- 0
						} else {
							tmp.out$include.new[j] <- max(tmp.out$include.new[1:(j-1)]) + 1
						}
					}
					#tmp.out$content <- NULL
					mon1 <- tmp.out[substr(tmp.out$time, 1, 7) < month,  ]
					mon2 <- tmp.out[substr(tmp.out$time, 1, 7) == month,  ]
					mon3 <- tmp.out[substr(tmp.out$time, 1, 7) > month,  ]
					if (nrow(mon2) >= 2) {
						for (k in 2:nrow(mon2)) {
							if (mon2$sim[k] > threshold) {
								mon2$include.new[k] <- mon2$include.new[k-1]
								mon2$include.new[k - 1] <- "Del"
							} else {
								mon2$include.new[k] <- "Union"
								mon2$include.new[k - 1] <- "Union"
							}
						}
						OUT[[i]] <- rbind(mon1, mon2, mon3)
					} else {
						OUT[[i]] <- tmp.out
					}
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


processDuplication <- function(datalist) {
	tryCatch({
				CONN <- .createConn()
				for (i in 1:length(datalist)) {
					united <- FALSE
					for (j in 1:nrow(datalist[[i]])) {
						if (datalist[[i]]$include.new[j] == 1) {
							next
						} else if (datalist[[i]]$include.new[j] == "Del") {
							strrm <- paste0("delete from comment_log where msgid ='", datalist[[i]]$msgid[j], "'")
							rs <- dbSendQuery(CONN, strrm)	
							dbClearResult(rs)
						} else if (datalist[[i]]$include.new[j] == "Union") {
							if (!united) {
								tmp.union <- datalist[[i]][datalist[[i]]$include.new == "Union", ]
								tmp.union <- tmp.union[order(tmp.union$time), ]
								tmp.str <- paste0(paste0("Added at ", tmp.union$time, ":\n"), paste0(tmp.union$content), collapse = "\n\n")
								for (k in 1:nrow(tmp.union)) {
									if (k == nrow(tmp.union)) {
										strsql <- paste0("update comment_log set include = 1, ", 
												"content = '", tmp.str, "'",
												" where msgid = '", tmp.union$msgid[k], "'")
										rs <- dbSendQuery(CONN, strsql)	
										dbClearResult(rs)
									} else {
										strsql <- paste0("update comment_log set include = 0", 
												" where msgid = '", tmp.union$msgid[k], "'")
										rs <- dbSendQuery(CONN, strsql)	
										dbClearResult(rs)
									}
								}
								united <- TRUE
							}
						} else {
							strsql <- paste0("update comment_log set include = ", datalist[[i]]$include.new[j], 
									" where msgid = '", datalist[[i]]$msgid[j], "'")
							rs <- dbSendQuery(CONN, strsql)	
							dbClearResult(rs)
						}
					}	
				}			
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}

listExtCom <- function(startdate = "2020-01-01") {	
	tryCatch({
				CONN <- .createConn()
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")
				tbl.com <- dbGetQuery(CONN, paste0("SELECT openid, title, time, msgid, include from comment_log where ", 
								"(include is null or include = 1) and time > '", startdate, "'"))		
				
				tbl.user$publicname <- .iconvutf8(tbl.user$publicname)
				tbl.com$title <- .iconvutf8(tbl.com$title)
				tbl.user$leavetime[is.na(tbl.user$leavetime)] <- "3028-08-01 10:01:50"
				tbl.user$leavetime <- strptime(tbl.user$leavetime, format = "%Y-%m-%d %H:%M:%S")
				tbl.user$jointime <- strptime(tbl.user$jointime, format = "%Y-%m-%d %H:%M:%S")
				tbl.com$time <- strptime(tbl.com$time, format = "%Y-%m-%d %H:%M:%S")
				for (i in 1:nrow(tbl.com)) {
					tmp.user <- tbl.user[tbl.user$openid == tbl.com$openid[i], ]
					if (nrow(tmp.user) == 0) {
						tbl.com$include[i] <- -1
					} else {
						tmp.user$timesign <- as.numeric(sign(tbl.com$time[i] - tmp.user$jointime)) * 
								as.numeric(sign(tmp.user$leavetime - tbl.com$time[i]))
						if (!any(tmp.user$timesign == 1)) {
							tbl.com$include[i] <- -1
						}
					}
				}
				OUT <- tbl.com[tbl.com$include %in% c(-1), ]
				tbl.followers <- dbGetQuery(CONN, paste0("SELECT openid, nickname from followers where openid in (", 
								paste0("'", unique(OUT$openid), "'", collapse = ", "), ")"))
				tbl.followers$nickname <- .iconvutf8(tbl.followers$nickname)
				OUT <- merge(OUT, tbl.followers, all.x = TRUE)
				return(OUT)			
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)	
}

updateExtCom <- function(MsgIDs) {
	tryCatch({
				CONN <- .createConn()
				for (i in 1:length(MsgIDs)) {
					strsql <- paste0("update comment_log set include = -1", 
							" where msgid = '", MsgIDs[i], "'")
					rs <- dbSendQuery(CONN, strsql)	
					dbClearResult(rs)
				}			
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
	
}