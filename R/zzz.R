# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# xiangma Version:", packageDescription("xiangma", fields = "Version")) )
}

