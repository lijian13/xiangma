
getNewOpenID <- function() {	
	tryCatch({
				CONN <- .createConn()

				tbl.user <- dbGetQuery(CONN, "SELECT * from groupmember")
				tbl.fans <- dbGetQuery(CONN, "SELECT * from followers")
				
				OUT <- tbl.fans[!tbl.fans$openid %in% tbl.user$openid, 1:10]
				Encoding(OUT$nickname) <- "UTF-8"
				Encoding(OUT$city) <- "UTF-8"
				Encoding(OUT$province) <- "UTF-8"
				Encoding(OUT$country) <- "UTF-8"
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
