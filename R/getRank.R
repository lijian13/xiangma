
getRank <- function() {	
	tryCatch({
				CONN <- .createConn()

				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember where status = 1")
				tbl.booklist <- dbGetQuery(CONN, "SELECT openid, count(openid) as total from comment_log group by openid")		
				Encoding(tbl.user$groupname) <- "UTF-8"
				Encoding(tbl.user$publicname) <- "UTF-8"
				
				OUT <- merge(tbl.booklist, tbl.user, all.x = TRUE)
				OUT <- OUT[!is.na(OUT$status) & OUT$status == 1, c("publicname", "total")]
				OUT <- OUT[order(OUT$total, decreasing = TRUE), ]
				rownames(OUT) <- 1:nrow(OUT)
				return(OUT)
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				try(dbDisconnect(CONN), silent = TRUE)
			}
	)
}
