
generateReport <- function(reportdate = substr(Sys.time(), 1, 11), reporttype = c("yearly", "monthly")) {
	reporttype <- match.arg(reporttype)
	tmpdir <- tempdir()
	file.copy(from = system.file("bookdown", reporttype, package = "xiangma"), to = tmpdir, recursive = TRUE)
	wkdir <- file.path(tmpdir, reporttype)
	old.wd <- getwd()
	setwd(wkdir)
	tryCatch({
				file.bd <- readLines("_bookdown.yml", encoding = "UTF-8")
				file.bd <- gsub("xiangma_yearly", paste0("xiangma_", substr(reportdate, 1, 4)), file.bd)
				zz <- file("_bookdown.yml", "w+", encoding = "UTF-8")
				writeLines(file.bd, zz)
				close(zz)
				
				require(bookdown)
				render_book("index.Rmd", "bookdown::pdf_book")
				outfile <- file.path(tmpdir, reporttype, "_book", "xiangma_yearly.pdf")
				if (file.exists(outfile)) {
					file.copy(from = outfile, to = old.wd)
				}
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				setwd(old.wd)
			}
	)
	
}

