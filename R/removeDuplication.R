
removeDuplication <- function(threshold = 0.8, remark = FALSE) {	
	tryCatch({
				CONN <- .createConn()
				commdf <- dbGetQuery(CONN, "select * from comment_log where doubanid in (select doubanid from comment_log group by openid, doubanid having count(openid) > 1)")

				d1 <- summarise (group_by(commdf, openid, doubanid), num = length(msgid))
				d2 <- d1[d1$num > 1, ]
				
				l1 <- lapply(1:nrow(d2), FUN = function(X) commdf[commdf$openid == d2$openid[X] & commdf$doubanid == d2$doubanid[X], c("msgid", "doubanid", "time", "title", "doubantitle", "include", "content")])
				l2 <- lapply(l1, FUN = function(X) {X$title <- toUTF8(X$title);X$doubantitle <- toUTF8(X$doubantitle);X$content <- toUTF8(X$content);X})
				l3 <- l2[sapply(l2, FUN = function(X) sum(X$include == 1) > 1)]
				
				OUT <- list()
				for (i in 1:length(l3)) {
					OUT[[i]] <- l3[[i]][order(l3[[i]]$time, decreasing = TRUE), ]
					OUT[[i]]$include[which(substr(OUT[[i]]$time, 1, 7) == substr(OUT[[i]]$time[1], 1, 7))] <- 0
					OUT[[i]]$include[1] <- 1
					OUT[[i]] <- l3[[i]][order(l3[[i]]$time), ]
					OUT[[i]]$sim <- 1
					OUT[[i]]$include.new <- OUT[[i]]$include
					for (j in 2:nrow(OUT[[i]])) {
						OUT[[i]]$sim[j] <- .similarity(OUT[[i]]$content[j], OUT[[i]]$content[1])
						OUT[[i]]$content <- NULL
						if (OUT[[i]]$include[j] == 1) {
							if (OUT[[i]]$sim[j] > threshold) {
								OUT[[i]]$include.new[j] <- 0
								if (identical(remark, TRUE)) {
									strsql <- paste0("update comment_log set include = 0 where msgid = '", OUT[[i]]$msgid[j], "'")
									rs <- dbSendQuery(CONN, strsql)	
									dbClearResult(rs)
								}
							} else {
								OUT[[i]]$include.new[j] <- 2
								if (identical(remark, TRUE)) {
									strsql <- paste0("update comment_log set include = 2 where msgid = '", OUT[[i]]$msgid[j], "'")
									rs <- dbSendQuery(CONN, strsql)	
									dbClearResult(rs)
								}
							}
						}
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
