############################ PACKAGES ###################################################
#########################################################################################
pkgL <- function(pckg){sapply(pckg, require, character.only=TRUE, quietly=TRUE)}
pkgL(c(
	"quantmod",
	"RQuantLib",
	"fields",
	"rgl",
	"lubridate",
	"timeSeries"
	))


############################ FUNS #######################################################
#########################################################################################
fun_fol <- paste0(dirname(sys.frame(1)$ofile),"/0_funs/")
funL 	<- function(files, dir=fun_fol){sapply(paste0(dir,files), source)}
funL(c(
	"0_basic.R", 
	"0_inst.R"
	))



############################ OTHER ######################################################
#########################################################################################
setwd(dirname(sys.frame(1)$ofile))
Sys.setenv(TZ='GMT')
