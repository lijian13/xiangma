
generateReport <- function(reportdate = substr(Sys.time(), 1, 11), reporttype = c("yearly", "monthly"), outtype = c("pdf", "word", "html", "all")) {
	reporttype <- match.arg(reporttype)
	outtype <- match.arg(outtype)
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
				file.idx <- readLines("index.Rmd", encoding = "UTF-8")
				file.idx <- gsub("YEAR", substr(reportdate, 1, 4), file.idx)
				file.idx <- gsub("DATE", reportdate, file.idx)
				zz <- file("index.Rmd", "w+", encoding = "UTF-8")
				writeLines(file.idx, zz)
				close(zz)
				
				require(bookdown)
				if (outtype %in% c("pdf", "all")) {
					render_book("index.Rmd", "bookdown::pdf_book")
					outfile <- file.path(tmpdir, reporttype, "_book", paste0("xiangma_", substr(reportdate, 1, 4), ".pdf"))
					if (file.exists(outfile)) {
						file.copy(from = outfile, to = old.wd, overwrite = TRUE)
					}
				}
				if (outtype %in% c("word", "all")) {
					render_book("index.Rmd", "bookdown::word_document2")
					outfile <- file.path(tmpdir, reporttype, "_book", paste0("xiangma_", substr(reportdate, 1, 4), ".docx"))
					if (file.exists(outfile)) {
						file.copy(from = outfile, to = old.wd, overwrite = TRUE)
					}
				}
				if (outtype %in% c("html", "all")) {
					render_book("index.Rmd", "bookdown::gitbook")
					outfile <- file.path(tmpdir, reporttype, "_book")
					if (file.exists(outfile)) {
						file.copy(from = outfile, to = old.wd, overwrite = TRUE, recursive = TRUE)
						file.rename(from = file.path(old.wd, "_book"), to =  file.path(old.wd, paste0("xiangma_", substr(reportdate, 1, 4))))
					}
				}		
			}, error = function(e) {
				returnstr <- gettext(e)
				cat(paste("Status :", returnstr))
			}, finally = {
				setwd(old.wd)
			}
	)
	
}

