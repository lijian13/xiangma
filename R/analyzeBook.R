
analyzeBook <- function(year = NULL, picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				
				if (is.null(year)) {
					commentdf <- dbGetQuery(CONN, "select msgid, doubanid as id, openid, week, time from comment_log where doubanid is null or doubanid not like 'LW%'")
				} else {
					commentdf <- dbGetQuery(CONN, paste0("select msgid, doubanid as id, openid, week, time from comment_log where time like '", year, 
									"%' and (doubanid is null or doubanid not like 'LW%')"))
				}
				
				booklistdf <- dbGetQuery(CONN, "select distinct id, author, tags from douban_list")
				Encoding(booklistdf$tags) <- "UTF-8"
				Encoding(booklistdf$author) <- "UTF-8"
				commentdf$month <- substr(commentdf$time, 1, 7)
				
				weekdf <- arrange(summarise(group_by(commentdf, week), num = length(msgid)), week)
				monthdf <- arrange(summarise(group_by(commentdf, month), num = length(msgid)), month)
				
				tagdf <- merge(commentdf[, c("id", "week", "month")], booklistdf, all.x = TRUE)
				tagdf <- tagdf[!is.na(tagdf$id), ]
				tagdf <- tagdf[!is.na(tagdf$tags), ]
				tagdf <- tagdf[nzchar(tagdf$tags), ]
				tagsdf1 <- lapply(1:nrow(tagdf), FUN = function(X) as.data.frame(do.call("rbind", strsplit(strsplit(tagdf[X, "tags"], split = ";")[[1]], split = ",")), stringsAsFactors = FALSE))
				tagsdf2 <- tagsdf1[sapply(tagsdf1 , ncol) == 2]
				tagsdf3 <- lapply(tagsdf2, FUN = function(X) {OUT <- X; OUT$V2 <- as.numeric(OUT$V2); OUT$w <- OUT$V2 / sum(OUT$V2);return(OUT)})
				tagsdf4 <- do.call("rbind", tagsdf3)
				tagsdf <- arrange(summarise(group_by(tagsdf4, V1), num = sum(w)), desc(num))
				tagvec <- tagsdf$num[1:20]
				names(tagvec) <- tagsdf$V1[1:20]				
				
				authordf <- arrange(summarise(group_by(tagdf, author), num = length(author)), desc(num))
				authordf$author <- strstrip(authordf$author)
				authordf <- authordf[nzchar(authordf$author), ]
				authordf$author <- gsub("\\[.*\\]", "", authordf$author)
				authordf$author <- gsub("\uFF08.*\uFF09", "", authordf$author)
				authordf$author <- gsub("\\(.*\\)", "", authordf$author)
				authordf$author <- gsub("\u3014.*\u3015", "", authordf$author)
				authordf$author <- gsub("\u3010.*\u3011", "", authordf$author)
				authordf$author <- gsub("\\s+", "", authordf$author)
				authorvec <- authordf$num[1:20]
				names(authorvec) <- authordf$author[1:20]

				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 800, height = 1200,
							units = "px", pointsize = 18, quality = 100, bg = "white", family = "")
					par(mfrow = c(4, 1))
					plot(num~week, data = weekdf, type = "l", xlab = "\u5468\u6570", ylab = "\u8BFB\u4E66\u91CF")
					if (is.null(year)) {
						plot(ts(monthdf$num, frequency = 12, start = c(2015, 11)), xlab = "\u6708\u4EFD", ylab = "\u8BFB\u4E66\u91CF")
					} else {
						plot(ts(monthdf$num, frequency = 12, start = c(as.numeric(year), 1)), xlab = "\u6708\u4EFD", ylab = "\u8BFB\u4E66\u91CF")
					}
					barplot(tagvec, las = 2, ylab = "\u6807\u7B7E\u70ED\u5EA6")
					barplot(authorvec, las = 2, ylab = "\u4F5C\u8005\u70ED\u5EA6")	
					dev.off()
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


