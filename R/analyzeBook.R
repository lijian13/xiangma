
analyzeBook <- function(year = NULL, picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				
				if (is.null(year)) {
					commentdf <- dbGetQuery(CONN, "select msgid, doubanid as id, openid, week, time from comment_log where include is null or include = 1")
				} else {
					commentdf <- dbGetQuery(CONN, paste0("select msgid, doubanid as id, openid, week, time from comment_log where time like '", year, 
									"%' and (include is null or include = 1)"))
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
				tagsdf1 <- suppressWarnings(lapply(1:nrow(tagdf), FUN = function(X) as.data.frame(do.call("rbind", strsplit(strsplit(tagdf[X, "tags"], split = ";")[[1]], split = ",")), stringsAsFactors = FALSE)))
				tagsdf2 <- tagsdf1[sapply(tagsdf1 , ncol) == 2]
				tagsdf3 <- lapply(tagsdf2, FUN = function(X) {OUT <- X; OUT$V2 <- as.numeric(OUT$V2); OUT$w <- OUT$V2 / sum(OUT$V2);return(OUT)})
				tagsdf4 <- do.call("rbind", tagsdf3)
				tagsdf <- arrange(summarise(group_by(tagsdf4, V1), num = sum(w)), desc(num))		
				
				tagdf$author <- strstrip(tagdf$author)
				tagdf <- tagdf[nzchar(tagdf$author), ]
				tagdf$author <- gsub("\\[.*\\]", "", tagdf$author)
				tagdf$author <- gsub("\uFF08.*\uFF09", "", tagdf$author)
				tagdf$author <- gsub("\\(.*\\)", "", tagdf$author)
				tagdf$author <- gsub("\u3014.*\u3015", "", tagdf$author)
				tagdf$author <- gsub("\u3010.*\u3011", "", tagdf$author)
				tagdf$author <- gsub("\\s+", "", tagdf$author)
				tagdf$author <- strstrip(tagdf$author)
				tagdf <- tagdf[nzchar(tagdf$author), ]
				
				authordf <- arrange(summarise(group_by(tagdf, author), num = length(author)), desc(num))
				authorvec <- authordf$num[1:20]
				names(authorvec) <- authordf$author[1:20]
				
				tagvec <- tagsdf$num[!tagsdf$V1 %in% tagdf$author]
				names(tagvec) <- tagsdf$V1[!tagsdf$V1 %in% tagdf$author]
				tagvec <- tagvec[1:20]

				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 800, height = 1200,
							units = "px", pointsize = 18, quality = 100, bg = "white", family = "")
					par(mfrow = c(4, 1), mar = c(9,5,1,2))
					plot(num~week, data = weekdf, type = "l", xlab = "\u5468\u6570", ylab = "\u8BFB\u4E66\u91CF")
					points(num~week, data = weekdf[which.max(weekdf$num[-nrow(weekdf)]), ], pch = 19 , cex = 1.5, col = "red")
					points(num~week, data = weekdf[which.min(weekdf$num[-nrow(weekdf)]), ], pch = 19 , cex = 1.5, col = "red")
					abline(h = tail(weekdf$num, n = 1), col = "red", lty = 2)
					if (is.null(year)) {
						tmp.ts <- ts(monthdf$num, frequency = 12, start = c(2015, 11))
						tmp.at <- seq(from = 2015+10/12, by = 1/12, length.out=length(tmp.ts))
						tmp.date <- substr(seq(as.Date("2015-11-01"), by = "month", length.out=length(tmp.ts)), 1, 7)
						tmp.idx <- ceiling(quantile(1:length(tmp.at), c(0,0.2,0.4,0.6,0.8,1)))
						plot(tmp.ts, axes = FALSE, xlab = "\u6708\u4EFD", ylab = "\u8BFB\u4E66\u91CF")
						axis(2)
						axis(1, labels = tmp.date[tmp.idx], at = tmp.at[tmp.idx])
						box() 	
					} else {
						tmp.ts <- ts(monthdf$num, frequency = 12, start = c(as.numeric(year), 1))
						tmp.at <- seq(from = as.numeric(year), by = 1/12, length.out=length(tmp.ts))
						tmp.date <- substr(seq(as.Date(paste0(year, "-01-01")), by = "month", length.out=length(tmp.ts)), 1, 7)
						tmp.idx <- ceiling(quantile(1:length(tmp.at), c(0,0.2,0.4,0.6,0.8,1)))
						plot(tmp.ts, axes = FALSE, xlab = "\u6708\u4EFD", ylab = "\u8BFB\u4E66\u91CF")
						axis(2)
						axis(1, labels = tmp.date[tmp.idx], at = tmp.at[tmp.idx])
						box() 
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


