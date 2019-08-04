

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
	wdf1 <- createWordFreq(segmentCN(toupper(strnm)), onlyCN = FALSE, nosymbol = FALSE, useStopDic = FALSE)	
	wlist <- lapply(freqlist$freq, FUN = function(X) {X$word <- toupper(X$word); names(X)[2] <- "freq2";wdf2 <- merge(wdf1, X, all.x = TRUE, all.y = TRUE);wdf2$freq[is.na(wdf2$freq)] <- 0;wdf2$freq2[is.na(wdf2$freq2)] <- 0;wdf2})
	OUT <- freqlist$df
	OUT$similarity <- sapply(wlist, FUN = function(X) crossprod(X$freq, X$freq2)[1,1] / (sum(X$freq^2)^0.5 * sum(X$freq2^2)^0.5))
	OUT <- OUT[order(OUT$similarity, decreasing = TRUE)[1:n], c("openid", "nickname", "similarity", "subscribe_time", "headimgurl")]
	return(OUT)
}

removeUsers <- function(wxobj, uids, leavetime = as.character(Sys.time())) {
	tryCatch({
		CONN <- .createConn()
		u100 <- getTagUsers(wxobj, 100)
		rmuids <- intersect(uids, u100)
		if (length(rmuids) > 0) {
			pmsg <- postUnTags(wxobj, tagid = 100, openids = rmuids)
		}
		for (i in 1:length(uids)) {
			strsql <- paste0("update member_log set leavetime = '", leavetime, 
					"', status = 0 where openid = '", uids[i], "' and status = 1")
			rs <- dbSendQuery(CONN, strsql)	
			dbClearResult(rs)							
		}
	}, error = function(e) {
		returnstr <- gettext(e)
		cat(paste("Status :", returnstr))
	}, finally = {
		try(dbDisconnect(CONN), silent = TRUE)
	})
}

addUsers <- function(wxobj, uids, unames = NA, jointime = as.character(Sys.time())) {
	tryCatch({
		CONN <- .createConn()
		fodf <- dbReadTable(CONN, "followers")
		mbdf <- dbReadTable(CONN, "member_log")
		mbdf <- mbdf[mbdf$status == 1, ]
		if (nrow(fodf) > 0) Encoding(fodf$nickname) <- "UTF-8"
		if (nrow(mbdf) > 0) Encoding(mbdf$publicname) <- "UTF-8"
		difuids <- setdiff(uids, unique(fodf$openid))
		if (length(difuids) > 0) {
			cat(paste0("No records in database:\n", paste0(difuids, collapse = "\n"), "\n"))
		}
		adduids <- intersect(uids, unique(fodf$openid))
		u100 <- getTagUsers(wxobj, 100)
		adduids_tag <- setdiff(adduids, u100)
		if (length(adduids_tag) > 0) {
			pmsg <- postTags(wxobj, tagid = 100, openids = adduids_tag)
		}
		adduids_db <- setdiff(adduids, unique(mbdf$openid))
		if (length(adduids_db) > 0) {
			adddf1 <- data.frame(openid = adduids_db, leavetime = NA, status = 1, stringsAsFactors = FALSE)
			adddf2 <- merge(adddf1, fodf[, c("openid", "nickname", "subscribe_time")], all.x = TRUE)
			adddf2$publicname <- adddf2$nickname
			if (!is.na(unames)) adddf2$publicname <- toUTF8(unames)
			adddf2$jointime <- adddf2$subscribe
			adddf2$jointime[substr(adddf2$jointime, 1, 10) != substr(Sys.time(), 1, 10)] <- jointime
			adddf3 <- adddf2[, names(mbdf)]
			dbWriteTable(CONN, "member_log", adddf3, row.names = FALSE, append = TRUE)
			cat(paste0(length(adduids_db), " group members have been added!\n"))
		}
	}, error = function(e) {
		returnstr <- gettext(e)
		cat(paste("Status :", returnstr))
	}, finally = {
		try(dbDisconnect(CONN), silent = TRUE)
	})	
}





