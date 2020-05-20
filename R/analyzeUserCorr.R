
analyzeUserCorr <- function(nm, year = substr(Sys.time(), 1, 4), picfile = NULL) {
	
}



.CorrJaccard <- function() {
	tryCatch({
				CONN <- .createConn()
				tbl.label <- dbGetQuery(CONN, "select openid, doubanid, doubantitle, content from comment_log where openid in (select openid from member_log where status = 1)")
				tbl.member <- dbGetQuery(CONN, "select openid, publicname from member_log where status = 1")	
				Encoding(tbl.label$doubantitle) <- "UTF-8"
				Encoding(tbl.label$content) <- "UTF-8"
				Encoding(tbl.member$publicname) <- "UTF-8"
				
				textdf <- summarise(group_by(tbl.label, openid), title = paste0(content, collapse = ". "))
				textdf <- textdf %>% left_join(tbl.member)
				w1 = segmentCN(textdf$title, returnType = "tm")
				d1 <- createDTM(w1)
				c1 <- as.matrix(1-ecodist::distance(d1, method = "jaccard"))
				rownames(c1) <- textdf$publicname
				colnames(c1) <- textdf$publicname
				
				library(showtext)
				library(Cairo)
				showtext_auto(enable = TRUE)
				font_add('SimSun', regular = 'C:/Windows/Fonts/simsun.ttc', italic = 'C:/Windows/Fonts/timesi.ttf')
				CairoPDF(file = "xm_corr1.pdf", family="SimSun", width = 20, height = 20)
				corrplot(c1, order = "AOE", cl.lim = c(0,1))
				dev.off()
				
				
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}

