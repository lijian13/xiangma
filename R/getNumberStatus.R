
getNumberStatus <- function(month = substr(Sys.time(), 1, 7), picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember where status = 1")
				tbl.bookhis <- dbGetQuery(CONN, "SELECT openid, count(openid) as total, sum(char_length(content)) as totalnchar from comment_log group by openid")	
				tbl.bookcur <- dbGetQuery(CONN, paste0("SELECT openid, count(openid) as curr from comment_log where time >= '", paste0(month, "-01 00:00:00"), "' group by openid"))
				Encoding(tbl.user$publicname) <- "UTF-8"
				Encoding(tbl.user$groupname) <- "UTF-8"
				
				if (nrow(tbl.bookcur) == 0) {
					tbl.bookcur <- data.frame(openid = character(), curr = numeric())
				}
				outdf <- merge(merge(tbl.user[, c("openid", "publicname", "jointime")], tbl.bookhis, all.x = TRUE), tbl.bookcur, all.x = TRUE)
				outdf$meanchar <- round(outdf$totalnchar / outdf$total, 0)
				outdf$days <- ceiling(as.numeric(difftime(Sys.time(), strptime(outdf$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days")))
				outdf$thismon <- 0
				outdf$thismon[substr(outdf$jointime, 1, 7) == substr(Sys.time(), 1, 7)] <- 1
				
				OUT <- select(outdf, publicname, curr, total, days, meanchar, thismon)
				OUT$curr[is.na(OUT$curr)] <- 0
				OUT$total[is.na(OUT$total)] <- 0
				OUT$meanchar[is.na(OUT$meanchar)] <- 0
				OUT <- arrange(OUT, desc(curr), desc(total), desc(days), desc(meanchar))
				
				df1 <- OUT[OUT$curr > 0, ]
				df2 <- OUT[OUT$curr == 0 & OUT$thismon == 1, ]
				df3 <- OUT[OUT$curr == 0 & OUT$thismon == 0, ]
				df1$safe <- 1
				df2$safe <- 1
				df3$safe <- 0
				
				OUTDF <- rbind(df1, df2, df3)
				OUTDF$thismon <- NULL
				rownames(OUTDF) <- NULL
				colnames(OUTDF) <- c("\u6635\u79F0", "\u5F53\u6708\u6570", "\u603B\u6570", "\u7FA4\u9F84(\u5929)", "\u4E66\u8BC4\u5747\u5B57", "safe")
				
				tmp.color <- rep("black", nrow(OUTDF))
				tmp.color[OUTDF$safe == 0] <- "red"
				OUTDF$safe <- NULL
				
				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 240 + max(nchar(OUTDF[,1], type = "width")) * 10, height = (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")
					g <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14, base_colour = tmp.color))
					grid.newpage()
					grid.draw(g)
					dev.off()
				}
				
				return(OUTDF)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
