
getRank <- function(month = substr(Sys.time(), 1, 7), picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")
				tbl.bookhis <- dbGetQuery(CONN, "SELECT openid, count(openid) as total, sum(char_length(content)) as totalnchar from comment_log where doubanid is null or doubanid not like 'LW%' group by openid")	
				tbl.bookcur <- dbGetQuery(CONN, paste0("SELECT openid, count(openid) as curr from comment_log where time like '", paste0(month, "%"), "' and (doubanid is null or doubanid not like 'LW%') group by openid"))
				Encoding(tbl.user$publicname) <- "UTF-8"
				
				if (nrow(tbl.bookcur) == 0) {
					tbl.bookcur <- data.frame(openid = character(), curr = numeric())
				}
				outdf <- merge(merge(tbl.user[tbl.user$status == 1, c("openid", "publicname", "jointime")], tbl.bookhis, all.x = TRUE), tbl.bookcur, all.x = TRUE)
				outdf$meanchar <- round(outdf$totalnchar / outdf$total, 0)
				
				tbl.user$leavetime[is.na(tbl.user$leavetime)] <- as.character(Sys.time())
				tbl.user$diffdays <- as.numeric(difftime(strptime(tbl.user$leavetime, format = "%Y-%m-%d %H:%M:%S"), strptime(tbl.user$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days"))
				tmp.daysdf <- summarise(group_by(tbl.user, openid), days = sum(diffdays, na.rm = TRUE))
				outdf <- merge(outdf, tmp.daysdf, all.x = TRUE)	
				#outdf$days <- ceiling(as.numeric(difftime(Sys.time(), strptime(outdf$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days")))
				
				
				outdf$thismon <- 0
				outdf$thismon[substr(outdf$jointime, 1, 7) == month] <- 1
				
				OUT <- select(outdf, publicname, curr, total, days, meanchar, thismon)
				OUT$curr[is.na(OUT$curr)] <- 0
				OUT$total[is.na(OUT$total)] <- 0
				OUT$meanchar[is.na(OUT$meanchar)] <- 0
				OUT <- arrange(OUT, desc(curr), desc(total), desc(days), desc(meanchar))
				OUT$rank <- 1:nrow(OUT)
				OUT <- OUT[, c(7, 1:6)]
				
				#df1 <- OUT[OUT$curr > 0, ]
				#df2 <- OUT[OUT$curr == 0 & OUT$thismon == 1, ]
				#df3 <- OUT[OUT$curr == 0 & OUT$thismon == 0, ]
				#df1$safe <- 1
				#df2$safe <- 1
				#df3$safe <- 0				
				#OUTDF <- rbind(df1, df2, df3)			
				OUTDF <- OUT
				OUTDF$safe <- 1
				OUTDF$safe[OUT$curr == 0 & OUT$thismon == 0] <- 0
				
				OUTDF$thismon <- NULL
				rownames(OUTDF) <- NULL
				OUTDF$days <- ceiling(OUTDF$days)
				colnames(OUTDF) <- c("\u6392\u540D", "\u6635\u79F0", "\u5F53\u6708\u6570", "\u603B\u6570", "\u7FA4\u9F84(\u5929)", "\u4E66\u8BC4\u5747\u5B57", "safe")
				
				tmp.color <- rep("black", nrow(OUTDF))
				tmp.color[OUTDF$safe == 0] <- "red"
				OUTDF$safe <- NULL
				
				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 400 + max(nchar(OUTDF[,1], type = "width")) * 10, height = 30 + (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")					
					g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14, base_colour = tmp.color))
					title1 <- textGrob(paste0(as.character(Sys.time()), " \u8BFB\u4E66\u8BB0\u5F55\n\uff08\u6CE8: \u7EA2\u8272\u5B57\u4F53\u8868\u793A\u672C\u6708\u5371\u9669\uff09"), 
							gp = gpar(fontsize=16))
					t1 <- gtable_add_rows(g1, heights = grobHeight(title1) + unit(5,"mm"), pos = 0)
					t1 <- gtable_add_grob(t1, title1, 1, 1, 1, ncol(t1))
					grid.newpage()
					grid.draw(t1)
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
