
getBookRank <- function(year = NULL, n = 20, picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				if (is.null(year)) {
					bookdf1 <- dbGetQuery(CONN, "select doubanid as id, doubantitle as title, count(doubanid) as num from comment_log where include = 1 group by doubanid, doubantitle")
					Encoding(bookdf1$title) <- "UTF-8"
					bookdf2 <- dbGetQuery(CONN, paste0("select doubanid as id, doubantitle as title, count(doubanid) as num1 from comment_log where time > '", 
									substr(Sys.time() - 365 * 24 * 3600, 1, 10), "' and include = 1 group by doubanid, doubantitle"))
					Encoding(bookdf2$title) <- "UTF-8"
					booklistdf <- dbGetQuery(CONN, "select distinct id, author from douban_list")
					Encoding(booklistdf$author) <- "UTF-8"
					
					bookdf <- merge(bookdf1, bookdf2[, c("id", "num1")], all.x = TRUE)
					bookdf$num1[is.na(bookdf$num1)] <- 0
					bookdf <- merge(bookdf, booklistdf, all.x = TRUE)
					bookdf <- bookdf[order(bookdf$num, bookdf$num1, decreasing = TRUE), ]
					bookdf$author <- sapply(strsplit(bookdf$author, split = ","), "[", 1)
					bookdf$author <- gsub("\\[.*\\]", "", bookdf$author)
					bookdf$author <- gsub("\uFF08.*\uFF09", "", bookdf$author)
					bookdf$author <- gsub("\\(.*\\)", "", bookdf$author)
					bookdf$author <- gsub("\u3014.*\u3015", "", bookdf$author)
					bookdf$author <- gsub("\u3010.*\u3011", "", bookdf$author)
					bookdf$author <- gsub("\\s+", "", bookdf$author)
					
					OUTDF <- bookdf[1:n, ]
					rownames(OUTDF) <- NULL
					OUTDF$id <- NULL
					colnames(OUTDF) <- c("\u4E66\u540D", "\u603B\u63A8\u8350\u6B21\u6570", "\u4E00\u5E74\u5185\u63A8\u8350\u6B21\u6570", "\u4F5C\u8005")
					
					if (!is.null(picfile)) {					
						jpeg(filename = picfile, 
								width = 210 + max(nchar(OUTDF[,1], type = "width")) * 8 + max(nchar(OUTDF[,4], type = "width")) * 8, 
								height = 60 + (nrow(OUTDF) + 1)*23, 
								units = "px", pointsize = 14, quality = 75, bg = "white", family = "")
						
						g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14))
						title1 <- textGrob(paste0("\u622A\u81F3\u76EE\u524D\u54CD\u9A6C\u70ED\u95E8\u4E66\u7C4D\u6392\u884C\u699C\n\uff08\u8BFB\u4E66\u603B\u6570\u4E3A ", 
										sum(bookdf1$num), " \u672C\uFF0C\u5176\u4E2D\u4E0D\u91CD\u590D\u4E66\u76EE ", nrow(bookdf1), " \u672C\uff09"), gp = gpar(fontsize=16))
						tail1 <- textGrob(paste0("\u66F4\u65B0\u4E8E ", as.character(Sys.time())), gp = gpar(fontsize=12))						
						t1 <- gtable_add_rows(g1, heights = grobHeight(title1) + unit(5,"mm"), pos = 0)
						t1 <- gtable_add_rows(t1, heights = grobHeight(tail1)+ unit(5,"mm"))
						t1 <- gtable_add_grob(t1, list(title1, tail1), t=c(1, nrow(t1)), l=c(1,2), r=ncol(t1))	
						grid.newpage()
						grid.draw(t1)	
						dev.off()
					}
				} else {
					bookdf1 <- dbGetQuery(CONN, paste0("select doubanid as id, doubantitle as title, count(doubanid) as num from comment_log where time like '",
									year, "%' and include = 1 group by doubanid, doubantitle"))
					Encoding(bookdf1$title) <- "UTF-8"
					bookdf2 <- dbGetQuery(CONN, paste0("select doubanid as id, doubantitle as title, count(doubanid) as num1 from comment_log where include = 1 group by doubanid, doubantitle"))
					Encoding(bookdf2$title) <- "UTF-8"
					booklistdf <- dbGetQuery(CONN, "select distinct id, author from douban_list")
					Encoding(booklistdf$author) <- "UTF-8"
					
					bookdf <- merge(bookdf1, bookdf2[, c("id", "num1")], all.x = TRUE)
					bookdf$num1[is.na(bookdf$num1)] <- 0
					bookdf <- merge(bookdf, booklistdf, all.x = TRUE)
					bookdf <- bookdf[order(bookdf$num, bookdf$num1, decreasing = TRUE), ]
					bookdf$author <- sapply(strsplit(bookdf$author, split = ","), "[", 1)
					bookdf$author <- gsub("\\[.*\\]", "", bookdf$author)
					bookdf$author <- gsub("\uFF08.*\uFF09", "", bookdf$author)
					bookdf$author <- gsub("\\(.*\\)", "", bookdf$author)
					bookdf$author <- gsub("\u3014.*\u3015", "", bookdf$author)
					bookdf$author <- gsub("\u3010.*\u3011", "", bookdf$author)
					bookdf$author <- gsub("\\s+", "", bookdf$author)
					
					OUTDF <- bookdf[1:n, ]
					rownames(OUTDF) <- NULL
					OUTDF$id <- NULL
					colnames(OUTDF) <- c("\u4E66\u540D", "\u5F53\u5E74\u63A8\u8350\u6B21\u6570", "\u603B\u63A8\u8350\u6B21\u6570", "\u4F5C\u8005")
					
					if (!is.null(picfile)) {					
						jpeg(filename = picfile, 
								width = 200 + max(nchar(OUTDF[,1], type = "width")) * 8 + max(nchar(OUTDF[,4], type = "width")) * 8, 
								height = 60 + (nrow(OUTDF) + 1)*23, 
								units = "px", pointsize = 14, quality = 75, bg = "white", family = "")
						
						g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14))
						title1 <- textGrob(paste0(year, "\u5E74\u54CD\u9A6C\u8BFB\u4E66\u7FA4\u70ED\u95E8\u4E66\u7C4D\u6392\u884C\u699C\n\uff08\u5F53\u5E74\u8BFB\u4E66\u603B\u6570\u4E3A ", 
										sum(bookdf1$num), " \u672C\uFF0C\u5176\u4E2D\u4E0D\u91CD\u590D\u4E66\u76EE ", nrow(bookdf1), " \u672C\uff09"), gp = gpar(fontsize=16))
						tail1 <- textGrob(paste0("\u66F4\u65B0\u4E8E ", as.character(Sys.time())), gp = gpar(fontsize=12))						
						t1 <- gtable_add_rows(g1, heights = grobHeight(title1) + unit(5,"mm"), pos = 0)
						t1 <- gtable_add_rows(t1, heights = grobHeight(tail1)+ unit(5,"mm"))
						t1 <- gtable_add_grob(t1, list(title1, tail1), t=c(1, nrow(t1)), l=c(1,2), r=ncol(t1))
						grid.newpage()
						grid.draw(t1)	
						dev.off()
					}
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
