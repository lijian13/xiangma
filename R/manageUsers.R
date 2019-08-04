

updateFollowers <- function(wxobj) {	
	
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
				
				tbl.all <- rbind(tbl.old, tbl.new)
				OUT <- list(df = tbl.all[, c("openid", "nickname", "subscribe_time", "headimgurl")])
				OUT$freq <- lapply(tbl.all$nickname, FUN = function(X) createWordFreq(segmentCN(X, nosymbol = FALSE), onlyCN = FALSE, nosymbol = FALSE, useStopDic = FALSE))
				invisible(OUT)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
	
}


searchUser <- function(strnm, freqlist, n = 5) {
	tryCatch({
		wdf1 <- createWordFreq(segmentCN(strnm), onlyCN = FALSE, nosymbol = FALSE, useStopDic = FALSE)	
		wlist <- lapply(freqlist$freq, FUN = function(X) {names(X)[2] <- "freq2";wdf2 <- merge(wdf1, X, all.x = TRUE, all.y = TRUE);wdf2$freq[is.na(wdf2$freq)] <- 0;wdf2$freq2[is.na(wdf2$freq2)] <- 0;wdf2})
		OUT <- freqlist$df
		OUT$similarity <- sapply(wlist, FUN = function(X) crossprod(X$freq, X$freq2)[1,1] / (sum(X$freq^2)^0.5 * sum(X$freq2^2)^0.5))
		OUT <- OUT[order(OUT$similarity, decreasing = TRUE)[1:n], c("openid", "nickname", "similarity", "subscribe_time", "headimgurl")]
		return(OUT)
	}, error = function(e) {
		returnstr <- gettext(e)
		cat(paste("Status :", returnstr))
	}, finally = {
		try(dbDisconnect(CONN), silent = TRUE)
	})
}