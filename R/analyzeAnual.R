
analyzeAnual <- function(nm, year = substr(Sys.time(), 1, 4), picfile = NULL) {
	tryCatch({
				CONN <- .createConn()
				
				userdf <- dbGetQuery(CONN, paste0("select * from member_log where publicname = '", toUTF8(nm), "'"))
				if (nrow(userdf) == 0) stop("There is no this user.")
				openid <- userdf$openid[1]
				commentdf <- dbGetQuery(CONN, paste0("select * from comment_log a left join (select distinct id, author, tags from douban_list) b on a.doubanid=b.id where openid = '", 
								openid, "' and time like '", year, "%' order by time"))		
				commentdf <- commentdf[!grepl("^LW", commentdf$doubanid), ]
				Encoding(commentdf$doubantitle) <- "UTF-8"
				Encoding(commentdf$title) <- "UTF-8"
				Encoding(commentdf$tags) <- "UTF-8"
				Encoding(commentdf$author) <- "UTF-8"
				Encoding(commentdf$content) <- "UTF-8"
				Encoding(userdf$publicname) <- "UTF-8"
				userdf$leavetime[is.na(userdf$leavetime)] <- format(Sys.time(), format = "")
				commentdf$no <- strpad(1:nrow(commentdf), 3, pad = "0")
				commentdf$author <- sapply(strsplit(commentdf$author, split = ","), "[", 1)
				commentdf$author <- gsub("\\[.*\\]", "", commentdf$author)
				commentdf$author <- gsub("\uFF08.*\uFF09", "", commentdf$author)
				commentdf$author <- gsub("\\(.*\\)", "", commentdf$author)
				commentdf$author <- gsub("\u3014.*\u3015", "", commentdf$author)
				commentdf$author <- gsub("\u3010.*\u3011", "", commentdf$author)
				commentdf$author <- gsub("\\s+", "", commentdf$author)
				commentdf <- do.call("rbind", lapply(split(commentdf, f = commentdf$doubanid), FUN = function(X) X[1, ]))
				
				out.n <- nrow(commentdf)
				out.wordcount <- sum(nchar(commentdf$content))
				out.meanword <- ceiling(out.wordcount / out.n)
				out.njoin <- nrow(userdf)
				out.nquit <- out.njoin - 1
				out.jointime <- substr(min(userdf$jointime),1 ,10)
				out.duration <- ceiling(sum(as.numeric(difftime(strptime(userdf$leavetime, format = "%Y-%m-%d %H:%M:%S"), strptime(userdf$jointime, format = "%Y-%m-%d %H:%M:%S"), unit = "days"))))
				
				out.str <- paste0("Hi ", userdf$publicname[1], "\uFF0C\u4F60\u4ECE ", out.jointime, 
						" \u52A0\u5165\u54CD\u9A6C\u8BFB\u4E66\u7FA4\u4EE5\u6765\uFF0C", 
						ifelse(out.njoin > 1, paste0("\u5386\u7ECF", out.njoin, "\u8FDB", out.nquit, "\u51FA\uFF0C"), ""),
						"\u5DF2\u7ECF\u5728\u672C\u7FA4\u594B\u6218 ", out.duration, " \u5929\uFF0C", "\u5728\u8FC7\u53BB\u7684", year, 
						"\u5E74\u91CC\u5171\u9605\u8BFB\u4E86 ", out.n, " \u672C\u4E66\uFF0C\u5171\u5199\u4E66\u8BC4 ", out.wordcount, " \u5B57\uFF0C\u5E73\u5747\u6BCF\u7BC7\u4E66\u8BC4 ",
						out.meanword, " \u5B57\u3002\u4EE5\u4E0B\u662F\u8BE6\u7EC6\u4E66\u76EE\uFF1A")
				
				out.book <- paste0(paste0(commentdf$no, ".\u300A", commentdf$title, "\u300B, ", commentdf$author), collapse = "\n")
				
				# tag data
				tagdf <- commentdf[!is.na(commentdf$tags), ]
				tagdf <- tagdf[nzchar(tagdf$tags), ]
				tagsdf1 <- lapply(1:nrow(tagdf), FUN = function(X) as.data.frame(do.call("rbind", strsplit(strsplit(tagdf[X, "tags"], split = ";")[[1]], split = ",")), stringsAsFactors = FALSE))
				tagsdf2 <- tagsdf1[sapply(tagsdf1 , ncol) == 2]
				tagsdf3 <- lapply(tagsdf2, FUN = function(X) {OUT <- X; OUT$V2 <- as.numeric(OUT$V2); OUT$w <- OUT$V2 / sum(OUT$V2);return(OUT)})
				tagsdf4 <- do.call("rbind", tagsdf3)
				tagsdf <- arrange(summarise(group_by(tagsdf4, V1), num = sum(w)), desc(num))
				tagvec <- tagsdf$num[1:6]
				names(tagvec) <- tagsdf$V1[1:6]
				
				# word clound
				wv <- unlist(segmentCN(commentdf$content, nature = TRUE))
				wdf <- createWordFreq(unlist(segmentCN(commentdf$content)))
				naturedf <- unique(data.frame(word = wv, nature = names(wv), stringsAsFactors = FALSE))
				wdf <- merge(wdf, naturedf, all.x = TRUE)
				wdf <- wdf[wdf$nature %in% c("an", "Ng", "n", "nr", "ns", "nt", "nz", "s", "Vg", "v", "vn", "l"), ]
				
				if (!is.null(picfile)) {
					OUT <- wdf
					# table
					OUTDF <- commentdf[, c("no", "title", "author")]
					names(OUTDF) <- c("\u5E8F\u53F7", "\u4E66\u540D", "\u4F5C\u8005")
					jpeg(filename = paste0(picfile, "1.jpg"), 
							width = 15+ max(nchar(OUTDF[,2], type = "width")) * 8 + max(nchar(OUTDF[,3], type = "width")) * 8, 
							height = 50 + (nrow(OUTDF) + 1)*23, 
							units = "px", pointsize = 14, quality = 75, bg = "white", family = "")
					
					g1 <- tableGrob(OUTDF, rows = NULL, theme = ttheme_default(base_size = 14))
					title1 <- textGrob("2017\u5E74\u9605\u8BFB\u4E66\u76EE", gp = gpar(fontsize=16))
					tail1 <- textGrob("\u6765\u81EA\u5FAE\u4FE1\u516C\u4F17\u53F7\uFF1A\u54CD\u9A6C\u8BFB\u4E66", gp = gpar(fontsize=12))
				
					t1 <- gtable_add_rows(g1, heights = grobHeight(title1) + unit(5,"mm"), pos = 0)
					t1 <- gtable_add_rows(t1, heights = grobHeight(tail1)+ unit(5,"mm"))
					t1 <- gtable_add_grob(t1, list(title1, tail1), t=c(1, nrow(t1)), l=c(1,2), r=ncol(t1))	
					grid.newpage()
					grid.draw(t1)	
					dev.off()
					# tags
					jpeg(filename = paste0(picfile, "2.jpg"), width = 1000, height = 600, units = "px", family = "Microsoft YaHei")
					barplot(tagvec, las = 1, ylab = "\u6807\u7B7E\u70ED\u5EA6")
					dev.off()
					
				} else {
					OUT <- paste0(out.str, "\n\n", out.book)
				}
				
				return(OUT)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}


