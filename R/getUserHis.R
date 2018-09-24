
getUserHis <- function(month = substr(Sys.time(), 1, 7), picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
			
				tbl.bookhis <- dbGetQuery(CONN, paste0("SELECT openid, count(openid) as total, sum(char_length(content)) as totalnchar from comment_log where time < '",  
								month, "-32' and (include is null or include = 1) group by openid"))		
				tbl.bookcur <- dbGetQuery(CONN, paste0("SELECT openid, count(openid) as curr from comment_log where time like '",  
								month, "%' and (include is null or include = 1) group by openid"))	

				outdf <- merge(tbl.bookcur, tbl.bookhis, all.x = TRUE)			
				tbl.user <- dbGetQuery(CONN, "SELECT * from member_log")
				tbl.user.old <- dbGetQuery(CONN, "SELECT * from groupmember")
				outdf <- rbind(merge(outdf[grepl("^TMP", outdf$openid),], unique(tbl.user.old[, c("openid", "publicname")]), all.x = TRUE),
						merge(outdf[!grepl("^TMP", outdf$openid),], unique(tbl.user[, c("openid", "publicname")]), all.x = TRUE))
				
				tbl.label <- dbGetQuery(CONN, paste0("select openid, doubanid from comment_log where time < '", 
								month, "-32' and openid in (", paste0("'", outdf$openid, "'", collapse = ","), ")"))
				tbl.douban <- dbGetQuery(CONN, "select id as doubanid, tags from douban_list")
				Encoding(outdf$publicname) <- "UTF-8"
				Encoding(tbl.user$publicname) <- "UTF-8"
				Encoding(tbl.douban$tags) <- "UTF-8"
				
				outdf$meanchar <- round(outdf$totalnchar / outdf$total, 0)

				# tag
				tbl.tags0 <- merge(tbl.label, tbl.douban, all.x = TRUE)
				tbl.tags0 <- tbl.tags0[!is.na(tbl.tags0$tags), ]
				tbl.tags0 <- tbl.tags0[nzchar(tbl.tags0$tags), ]
				tbl.tags <- summarise(group_by(tbl.tags0, openid), tags = paste0(.dealwithTags(tags)$tag, collapse = ","))
				outdf <- merge(outdf, tbl.tags, all.x = TRUE)
				outdf$tags[is.na(outdf$tags)] <- ""
				
				OUT <- select(outdf, publicname, curr, total, meanchar, tags)
				OUT$total[is.na(OUT$total)] <- 0
				OUT$meanchar[is.na(OUT$meanchar)] <- 0
				OUT <- arrange(OUT, desc(total), desc(curr), desc(meanchar))
				OUT$rank <- 1:nrow(OUT)
		
				OUTDF <- OUT[, c(6, 1:5)]
				rownames(OUTDF) <- NULL
				colnames(OUTDF) <- c("\u6392\u540D", "\u6635\u79F0", "\u5F53\u6708\u6570", "\u603B\u6570", "\u4E66\u8BC4\u5747\u5B57", "\u70ED\u95E8\u6807\u7B7E")
				
				if (!is.null(picfile)) {					
					jpeg(filename = picfile, width = 200 + max(nchar(OUTDF[,2], type = "width")) * 8 + max(nchar(OUTDF[,6], type = "width")) * 8, height = 30 + (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")					
					g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14, base_colour = "black"))
					title1 <- textGrob(paste0(month, " \u54CD\u9A6C\u8BFB\u4E66\u7FA4\u53CB\u6392\u884C\u699C \uFF08\u5F53\u6708\u8BFB\u4E66\uFF1A", sum(OUTDF[[3]]),"\uFF09"), 
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
