
getBookRank <- function(month = NULL, n = 20, picfile = NULL) {
	tryCatch({
				CONN <- .createConn() 
				if (!is.null(month) && grepl("^20[0-9][0-9]$", month)) {
					 strsql1 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num, ", 
							 "avg(date_part('epoch', current_timestamp - to_timestamp(time, 'yyyy-mm-dd hh24:mi:ss'))::NUMERIC / 86400) as age ", 
							 "from comment_log where time like '", month, "%' and include = 1 group by doubanid, doubantitle")
					 strsql2 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num1 from comment_log ", 
							 "where include = 1 group by doubanid, doubantitle")
					 strnm <- c("\u4E66\u540D", "\u5F53\u5E74\u63A8\u8350\u6B21\u6570", "\u603B\u63A8\u8350\u6B21\u6570", "\u4F5C\u8005") 
					 strtitle <- paste0(month, "\u5E74\u54CD\u9A6C\u8BFB\u4E66\u7FA4\u70ED\u95E8\u4E66\u7C4D\u6392\u884C\u699C", 
							 "\n\uff08\u5F53\u5E74\u8BFB\u4E66\u603B\u6570\u4E3A ")
							 
				} else if (!is.null(month) && grepl("^20[0-9][0-9]-[0-1][0-9]$", month)) {
					strsql1 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num, ", 
							"avg(date_part('epoch', current_timestamp - to_timestamp(time, 'yyyy-mm-dd hh24:mi:ss'))::NUMERIC / 86400) as age ", 
							"from comment_log where time like '", month, "%' and include = 1 group by doubanid, doubantitle")
					strsql2 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num1 from comment_log ", 
							"where include = 1 group by doubanid, doubantitle")
					strnm <- c("\u4E66\u540D", "\u5F53\u6708\u63A8\u8350\u6B21\u6570", "\u603B\u63A8\u8350\u6B21\u6570", "\u4F5C\u8005") 
					strtitle <- paste0(month, "\u6708\u54CD\u9A6C\u8BFB\u4E66\u7FA4\u70ED\u95E8\u4E66\u7C4D\u6392\u884C\u699C", 
							"\n\uff08\u5F53\u6708\u8BFB\u4E66\u603B\u6570\u4E3A ")
				} else {
					strsql1 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num, ", 
							"avg(date_part('epoch', current_timestamp - to_timestamp(time, 'yyyy-mm-dd hh24:mi:ss'))::NUMERIC / 86400) as age ", 
							"from comment_log where include = 1 group by doubanid, doubantitle")
					strsql2 <- paste0("select doubanid as id, doubantitle as title, count(doubanid) as num1 from comment_log where time > '", 
							substr(Sys.time() - 365 * 24 * 3600, 1, 10), "' and include = 1 group by doubanid, doubantitle")
					strnm <- c("\u4E66\u540D", "\u603B\u63A8\u8350\u6B21\u6570", "\u4E00\u5E74\u5185\u63A8\u8350\u6B21\u6570", "\u4F5C\u8005")
					strtitle <- paste0("\u622A\u81F3\u76EE\u524D\u54CD\u9A6C\u70ED\u95E8\u4E66\u7C4D\u6392\u884C\u699C", 
							"\n\uff08\u8BFB\u4E66\u603B\u6570\u4E3A ")
				}
				
				bookdf1 <- dbGetQuery(CONN, strsql1)
				Encoding(bookdf1$title) <- "UTF-8"
				bookdf2 <- dbGetQuery(CONN, strsql2)
				Encoding(bookdf2$title) <- "UTF-8"
				booklistdf <- dbGetQuery(CONN, "select distinct id, author, title as title1 from douban_list")
				Encoding(booklistdf$author) <- "UTF-8"
				Encoding(booklistdf$title1) <- "UTF-8"
				
				bookdf <- merge(bookdf1, bookdf2[, c("id", "title", "num1")], all.x = TRUE)
				bookdf$num1[is.na(bookdf$num1)] <- 0
				bookdf <- merge(bookdf, booklistdf, all.x = TRUE)
				bookdf$title <- bookdf$title1
				bookdf$title1 <- NULL
				bookdf <- bookdf[order(bookdf$num, bookdf$num1, -bookdf$age, decreasing = TRUE), ]
				bookdf$author <- .cleanAuthors(bookdf$author)
				
				OUTDF <- bookdf[1:min(n, nrow(bookdf)), ]
				rownames(OUTDF) <- NULL
				OUTDF$id <- NULL
				OUTDF$age <- NULL
				colnames(OUTDF) <- strnm
				
				if (!is.null(picfile)) {
					strtitle <-  paste0(strtitle, sum(bookdf1$num), 
							" \u672C\uFF0C\u5176\u4E2D\u4E0D\u91CD\u590D\u4E66\u76EE ", 
							nrow(bookdf1), " \u672C\uff09")
					jpeg(filename = picfile, 
							width = 210 + max(nchar(OUTDF[,1], type = "width")) * 8 + max(nchar(OUTDF[,4], type = "width")) * 8, 
							height = 60 + (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")
					
					g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14))
					title1 <- textGrob(strtitle, gp = gpar(fontsize=16))
					tail1 <- textGrob(paste0("\u66F4\u65B0\u4E8E ", as.character(Sys.time())), gp = gpar(fontsize=12))						
					t1 <- gtable_add_rows(g1, heights = grobHeight(title1) + unit(5,"mm"), pos = 0)
					t1 <- gtable_add_rows(t1, heights = grobHeight(tail1)+ unit(5,"mm"))
					t1 <- gtable_add_grob(t1, list(title1, tail1), t=c(1, nrow(t1)), l=c(1,2), r=ncol(t1))	
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
