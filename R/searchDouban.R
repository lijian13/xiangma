

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



searchDoubanBook <- function(id, detail = FALSE) {
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






