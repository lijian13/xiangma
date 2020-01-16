

searchDoubanList <- function(q) {
	require(rvest)
	q <- gsub("\\s+", "+", strstrip(q))
	html0 <- read_html(paste0("https://www.douban.com/search?q=", q))
	node1 <- html_nodes(html0, xpath = "//div[@class='result-list'][1]/div[@class='result']/div[@class='content']/div[@class='title']/h3")
	s1 <- strstrip(html_text(node1))
	node2 <- html_nodes(html0, xpath = "//div[@class='result-list'][1]/div[@class='result']/div[@class='content']/div[@class='title']/div/span[@class='subject-cast']")
	s2 <- strstrip(html_text(node2))
	node3 <- html_nodes(html0, xpath = "//div[@class='result-list'][1]/div[@class='result']/div[@class='content']/p")
	s3 <- strstrip(html_text(node3))
	node4 <- html_nodes(html0, xpath = "//div[@class='result-list'][1]/div[@class='result']/div[@class='content']/div[@class='title']/h3/a")
	s4 <- strstrip(gsub(",\\s+qcat:.*$", "", gsub("^.*sid:", "", html_attr(node4, "onclick"))))
	n0 <- min(length(s1), length(s2), length(s3), length(s4))
	OUT <- data.frame(q = q, id = s4[1:n0], title = s1[1:n0], author = s2[1:n0], desc = s3[1:n0], stringsAsFactors = FALSE)
	OUT$title <- gsub("\u00A0", "", OUT$title)
	return(OUT)
}




searchDoubanBook <- function(id, detail = FALSE) {
	require(rvest)
	html0 <- read_html(paste0("https://book.douban.com/subject/", id))
	node1 <- html_nodes(html0, xpath = "//div[@id='wrapper']/h1")
	s1 <- strstrip(html_text(node1))
	node2 <- html_nodes(html0, xpath = "//div[@id='wrapper']/div[@id='content']//div[@class='indent']//div[@id='info']/span/a")
	s2 <- strstrip(html_text(node2))
	node3 <- html_nodes(html0, xpath = "//div[@id='wrapper']/div[@id='content']//div[@class='indent']//div[@id='info']/span")
	s3 <- strstrip(gsub(":.*$", ":", html_text(node3)))
	node4 <- html_nodes(html0, xpath = "//div[@id='wrapper']/div[@id='content']//div[@class='indent']//div[@id='info']")
	s4 <- strstrip(html_text(node4))
	node5 <- html_nodes(html0, xpath = "//div[@class='related_info']//div[@id='db-tags-section']/div[@class='indent']/span")
	s5 <- strstrip(html_text(node5))
	node6 <- html_nodes(html0, xpath = "//div[@class='rating_self clearfix']//div[@class='rating_sum']//a[@class='rating_people']/span")
	s6 <- strstrip(html_text(node6))
	node7 <- html_nodes(html0, xpath = "//div[@class='rating_self clearfix']/strong[@property='v:average']")
	s7 <- strstrip(html_text(node7))
	node8 <- html_nodes(html0, xpath = "//div[@class='related_info']/div[@id='link-report']//div[@class='intro']/p")
	s8 <- strstrip(html_text(node8))
	
	infodf <- data.frame(var = s3, value = strsplit(s4, paste0(s3, collapse = "|"))[[1]][-1], stringsAsFactors = FALSE)
	infodf$value <- sapply(strsplit(gsub("/", "", infodf$value), "\\s+"), FUN = function(X) paste0(X[-1], collapse = ","))
	
	OUT <- data.frame(id = id,
			title = s1,
			subtitle = ifelse("\u526F\u6807\u9898:" %in% infodf$var, infodf$value[infodf$var == "\u526F\u6807\u9898:"], ""),
			author = ifelse("\u4F5C\u8005:" %in% infodf$var, infodf$value[infodf$var == "\u4F5C\u8005:"], ""),
			translator = ifelse("\u8BD1\u8005:" %in% infodf$var, infodf$value[infodf$var == "\u8BD1\u8005:"], ""),
			pubdate = ifelse("\u51FA\u7248\u5E74:" %in% infodf$var, infodf$value[infodf$var == "\u51FA\u7248\u5E74:"], ""),
			publisher = ifelse("\u51FA\u7248\u793E:" %in% infodf$var, infodf$value[infodf$var == "\u51FA\u7248\u793E:"], ""),
			pages =ifelse("\u9875\u6570:" %in% infodf$var, infodf$value[infodf$var == "\u9875\u6570:"], ""),
			price = as.numeric(gsub("[^0-9\\.]", "", ifelse("\u5B9A\u4EF7:" %in% infodf$var, infodf$value[infodf$var == "\u5B9A\u4EF7:"], ""))),
			isbn10 = "",
			isbn13 = ifelse("ISBN:" %in% infodf$var, infodf$value[infodf$var == "ISBN:"], ""),
			ratingnum = ifelse(length(s6) > 0, as.numeric(s6), 0),
			ratingavg = ifelse(length(s7) > 0, as.numeric(s6), 0),
			seriesid = NA,
			seriestitle = ifelse("\u4E1B\u4E66:" %in% infodf$var, infodf$value[infodf$var == "\u4E1B\u4E66:"], ""),
			binding = ifelse("\u05F0\u05A1:" %in% infodf$var, infodf$value[infodf$var == "\u05F0\u05A1:"], ""),
			origin_title = NA,
			image = NA,
			alt = NA,
			tags = ifelse(length(s5) > 0, paste0(paste(s5, length(s5):1, sep = ","), collapse = ";"), NA),
			catalog = NA,
			author_intro = NA,
			summary = ifelse(length(s8) > 0, paste0(s8, collapse = "\n"), NA),	
			stringsAsFactors = FALSE)

	if (identical(detail, FALSE)) {
		OUT <- OUT[, c("id", "title", "subtitle", "author", "pubdate", "translator", "publisher", "pages", "price", "isbn10", "isbn13", "ratingnum", "ratingavg")]
	}
	return(OUT)	
}

# api (old)
searchDoubanBook.api <- function(id, detail = FALSE) {
	out.json <- getForm(paste0("https://api.douban.com/v2/book/", id))
	out.list <- fromJSON(out.json)
	
	OUT <- data.frame(id = out.list$id,
			title = out.list$title,
			subtitle = .transChar(out.list$subtitle),
			author = paste0(out.list$author, collapse = ","),
			translator = paste0(Rweixin:::.transChar(out.list$translator), collapse = ","),
			pubdate = .transChar(out.list$pubdate),
			publisher = .transChar(out.list$publisher),
			pages = as.numeric(out.list$pages),
			price = as.numeric(out.list$price),
			isbn10 = .transChar(out.list$isbn10),
			isbn13 = .transChar(out.list$isbn13),
			ratingnum = as.numeric(out.list$rating$numRaters),
			ratingavg = as.numeric(out.list$rating$average),
			seriesid = .transChar(out.list$series$id),
			seriestitle = .transChar(out.list$series$title),
			binding = .transChar(out.list$binding),
			origin_title = .transChar(out.list$origin_title),
			image = .transChar(out.list$image),
			alt = .transChar(out.list$alt),
			tags = paste0(paste(out.list$tags$title, out.list$tags$count, sep = ","), collapse = ";"),
			catalog = .transChar(out.list$catalog),
			author_intro = .transChar(out.list$author_intro),
			summary = .transChar(out.list$summary),	
			stringsAsFactors = FALSE)
	
	if (identical(detail, FALSE)) {
		OUT <- OUT[, c("id", "title", "subtitle", "author", "pubdate", "translator", "publisher", "pages", "price", "isbn10", "isbn13", "ratingnum", "ratingavg")]
	}
	return(OUT)	
}


searchDouban <- function(q, count = 20, detail = FALSE) {
	out.json <- getForm("https://api.douban.com/v2/book/search", q = toUTF8(q), count = count)
	out.list <- try(fromJSON(out.json)$books, silent = TRUE)
	if (inherits(out.list, "try-error") || length(out.list) == 0) return(data.frame())
	OUT <- data.frame(id = out.list$id,
			title = out.list$title,
			subtitle = .transChar(out.list$subtitle),
			author = sapply(out.list$author, paste0, collapse = ","),
			translator = sapply(sapply(out.list$translator, Rweixin:::.transChar), paste0, collapse = ","),
			pubdate = .transChar(out.list$pubdate),
			publisher = .transChar(out.list$publisher),
			pages = as.numeric(out.list$pages),
			price = as.numeric(out.list$price),
			isbn10 = .transChar(out.list$isbn10),
			isbn13 = .transChar(out.list$isbn13),
			ratingnum = as.numeric(out.list$rating$numRaters),
			ratingavg = as.numeric(out.list$rating$average),
			seriesid = .transChar(out.list$series$id),
			seriestitle = .transChar(out.list$series$title),
			binding = .transChar(out.list$binding),
			origin_title = .transChar(out.list$origin_title),
			image = .transChar(out.list$image),
			alt = .transChar(out.list$alt),
			tags = sapply(out.list$tags, FUN = function(X) paste0(paste(X$title, X$count, sep = ","), collapse = ";")),
			catalog = .transChar(out.list$catalog),
			author_intro = .transChar(out.list$author_intro),
			summary = .transChar(out.list$summary),	
			stringsAsFactors = FALSE)
	if (identical(detail, FALSE)) {
		OUT <- OUT[, c("id", "title", "subtitle", "author", "translator", "pubdate", "publisher", "pages", "price", "isbn10", "isbn13", "ratingnum", "ratingavg")]
	}
	return(OUT)	
}





