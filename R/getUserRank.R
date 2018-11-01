
getUserRank <- function(picfile = NULL, newweight = FALSE) {
	tryCatch({
				CONN <- .createConn()
				
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")
				tbl.commentlog <- dbGetQuery(CONN, "select openid, doubanid, time, char_length(content) as nchar, include from comment_log where openid in (select openid from member_log where status = 1)")
				mintime <- strptime("2015-11-25 11:38:00", "%Y-%m-%d %H:%M:%S")
				maxtime <- Sys.time()
				tbl.commentlog <- tbl.commentlog[tbl.commentlog$include %in% c(NA, 1), ]
				tbl.commentlog$score <- as.numeric(difftime(strptime(tbl.commentlog$time, "%Y-%m-%d %H:%M:%S"), mintime, units = "days")) / as.numeric(difftime(maxtime, mintime, units = "days"))
				if (identical(newweight, TRUE)) {	
					tbl.bookhis <- summarise(group_by(tbl.commentlog, openid), total = length(openid), totalnchar = sum(nchar), score = sum(score))
					tbl.label <- tbl.commentlog[, c("openid", "doubanid")]
				} else {
					tbl.bookhis <- dbGetQuery(CONN, "SELECT openid, count(openid) as total, sum(char_length(content)) as totalnchar from comment_log where include is null or include = 1 group by openid")		
					tbl.label <- dbGetQuery(CONN, "select openid, doubanid from comment_log where openid in (select openid from member_log where status = 1)")
				}	
				tbl.douban <- dbGetQuery(CONN, "select id as doubanid, tags from douban_list")
				Encoding(tbl.user$publicname) <- "UTF-8"
				Encoding(tbl.douban$tags) <- "UTF-8"
				
				outdf <- merge(tbl.user[tbl.user$status == 1, c("openid", "publicname", "jointime")], tbl.bookhis, all.x = TRUE)
				outdf$meanchar <- round(outdf$totalnchar / outdf$total, 0)
				
				tbl.user$leavetime[is.na(tbl.user$leavetime)] <- as.character(Sys.time())
				tbl.user$diffdays <- as.numeric(difftime(strptime(tbl.user$leavetime, format = "%Y-%m-%d %H:%M:%S"), strptime(tbl.user$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days"))
				tmp.daysdf <- summarise(group_by(tbl.user, openid), days = sum(diffdays, na.rm = TRUE))
				outdf <- merge(outdf, tmp.daysdf, all.x = TRUE)
				outdf$dura <- round(outdf$days / outdf$total, 1)
				outdf$dura[is.na(outdf$dura)] <- 0
				
				# tag
				tbl.tags0 <- merge(tbl.label, tbl.douban, all.x = TRUE)
				tbl.tags0 <- tbl.tags0[!is.na(tbl.tags0$tags), ]
				tbl.tags0 <- tbl.tags0[nzchar(tbl.tags0$tags), ]
				tbl.tags <- summarise(group_by(tbl.tags0, openid), tags = paste0(.dealwithTags(tags)$tag, collapse = ","))
				outdf <- merge(outdf, tbl.tags, all.x = TRUE)
				outdf$tags[is.na(outdf$tags)] <- ""
				
				if (identical(newweight, TRUE)) {	
					OUT <- select(outdf, publicname, total, days, meanchar, dura, tags, score)
					OUT$total[is.na(OUT$total)] <- 0
					OUT$meanchar[is.na(OUT$meanchar)] <- 0
					OUT <- arrange(OUT, desc(score), desc(total), desc(days), desc(meanchar), dura)
					OUT$rank <- 1:nrow(OUT)
					OUTDF <- OUT[, c(8, 1:6)]
					
				} else {
					OUT <- select(outdf, publicname, total, days, meanchar, dura, tags)
					OUT$total[is.na(OUT$total)] <- 0
					OUT$meanchar[is.na(OUT$meanchar)] <- 0
					OUT <- arrange(OUT, desc(total), desc(days), desc(meanchar), dura)
					OUT$rank <- 1:nrow(OUT)
					OUTDF <- OUT[, c(7, 1:6)]
				}	
				
				rownames(OUTDF) <- NULL
				OUTDF$days <- ceiling(OUTDF$days)
				colnames(OUTDF) <- c("\u6392\u540D", "\u6635\u79F0", "\u603B\u6570", "\u7FA4\u9F84(\u5929)", "\u4E66\u8BC4\u5747\u5B57", "\u6BCF\u4E66\u5929\u6570", "\u70ED\u95E8\u6807\u7B7E")
				
				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 300 + max(nchar(OUTDF[,2], type = "width")) * 8 + max(nchar(OUTDF[,7], type = "width")) * 8, height = 30 + (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")					
					g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14, base_colour = "black"))
					title1 <- textGrob(paste0(as.character(Sys.time()), " \u54CD\u9A6C\u8BFB\u4E66\u7FA4\u53CB\u6392\u884C\u699C"), 
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
