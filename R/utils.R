


.createDBConfig <- function(host = "localhost", port = "5432", dbname = "xiangma", user = "", password = "") {
	connpath <- file.path(Sys.getenv("APPDATA"), "xiangma", "db")
	if (!file.exists(connpath)) dir.create(connpath, recursive = TRUE)
	objlist <- list(host = host, port = port, dbname = dbname, user = user, password = password)	
	objfile <- file(file.path(connpath, "config.json") , open = "w" )
	writeLines(toJSON(objlist), objfile)
	close(objfile)
	invisible(TRUE)
}


.createConn <- function() {
	connpath <- file.path(Sys.getenv("APPDATA"), "xiangma", "db")
	if (!file.exists(file.path(connpath, "config.json"))) {
		stop("Please use '.createDBConfig' to set the config file!")
	}
	conlist <- fromJSON(file.path(connpath, "config.json"))
	CONN <- dbConnect(dbDriver("PostgreSQL"), user = conlist$user, password = conlist$password, dbname = conlist$dbname, host = conlist$host, port = conlist$port)
	return(CONN)
}

