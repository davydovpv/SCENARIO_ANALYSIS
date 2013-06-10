################# CHECKERS ###############################################################
##########################################################################################
check_strings <- function(...){
	for (string in list(...)){
		if ( missing(string) || !is(string, "character") ) stop(paste("check", string))}}

check_numerics <- function(...){
	for (numeric in list(...)){
		if ( missing(numeric) || !is(numeric, "numeric") ) stop(paste("check", numeric))}}

check_date <- function(date){
	try(date <- as.POSIXct( time_match(date) ))
	if( !is(date, "POSIXct") ) stop(paste("check", date))
	date}

check_currency 	<- function(cur){
	cur 	<- tolower(cur)
	if (! cur %in% c("eur","usd","gbp","jpy","aud","cad") ) stop("currency is not supported")
	cur}

check_mode <- function(mode){
	# 0 - "call", 1 - "put"
	mode <- tolower(mode)
	if (!mode %in% c(0, 1)){ stop("mode must be \"0\" or \"1\"" )}		
	mode}

check_type <- function(type){
	# 0 - "euro", 1 - "ame"
	if (missing(type)) {type="euro";cat("using default type: euro\n")}
	type 	<- tolower(type)
	if (!type %in% c(0, 1)) stop("type must be \"0\" or \"1\"")
	type}

check_class <- function(class){
	if (missing(class)) {class="vanilla";cat("using default class: vanilla\n")}
	class 	<- tolower(class)
	if (! class %in% c("vanilla") ) stop("class is not supported")
	class}

################# GEN_PAR ################################################################
##########################################################################################
attach(what=NULL,name="gen.par")
lsg <- function(){ls("gen.par")}

vaGenPar 	<- function(date.start, date.end, r, cal, nyear=252){
	rm(list=ls("gen.par"),envir=as.environment("gen.par"))
	temp <- list(date.start=date.start, date.end=date.end, cal=cal, r=r, nyear=nyear)
	class(temp) <- c("gen.par")
	assign("gen.par",temp,envir=as.environment("gen.par"))
    cat("<---- gen.par ---->\n")}

print.gen.par <- function(x){utils::str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,give.length=FALSE, give.attr=FALSE, indent.str="");invisible(x)}
g.date.start <- function(){get("gen.par",envir=as.environment("gen.par"))[["date.start"]]}
g.date.end <- function(x){get("gen.par",envir=as.environment("gen.par"))[["date.end"]]}
g.cal <- function(x){get("gen.par",envir=as.environment("gen.par"))[["cal"]]}
g.r <- function(x){get("gen.par",envir=as.environment("gen.par"))[["r"]]}
g.nyear <- function(x){get("gen.par",envir=as.environment("gen.par"))[["nyear"]]}


################# STOCK_PAR ##############################################################
##########################################################################################
attach(what=NULL,name="stock.par")
lss <- function(){ls("stock.par")}
rms <- function(){rm(list=ls("stock.par"),envir=as.environment("stock.par"))}

vaStockPars <- function(){
	objects <- ls("stock.par"); temp <- list()
	for(i in objects){temp[[length(temp)+1]]=get(i,envir=as.environment("stock.par"))}
	temp}

vaStockPar 	<- function(ticker, s0, d=0, date, vol){
	# volatility surface should be included here
	temp <- list(ticker=ticker, s0=s0, d=d, date=date, vol=vol)
	class(temp) <- c("stock.par")
	assign(paste0(ticker,".par"),temp,envir=as.environment("stock.par"))
    cat("<----", ticker,".par ---->\n")}

print.stock.par <- function(x){utils::str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,give.length=FALSE, give.attr=FALSE, indent.str="");invisible(x)}
s.ticker <- function(x){if(!is(x,"stock.par")) stop("wrong object."); x[["ticker"]]}
s.s0 <- function(x){if(!is(x,"stock.par")) stop("wrong object."); x[["s0"]]}
s.d <- function(x){if(!is(x,"stock.par")) stop("wrong object."); x[["d"]]}
s.date <- function(x){if(!is(x,"stock.par")) stop("wrong object."); x[["date"]]}
s.vol <- function(x){if(!is(x,"stock.par")) stop("wrong object."); x[["vol"]]}



################# SECURITIES #############################################################
##########################################################################################
attach(what=NULL,name="securities")
lsi <- function(){ls("securities")}
rmi <- function(){rm(list=ls("securities"),envir=as.environment("securities"))}
vaSecurities <- function(){
	objects=ls("securities"); temp <- list()
	for(i in objects){temp[[length(temp)+1]]=get(i,envir=as.environment("securities"))}
	temp}

print.security <- function(x){
	utils::str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,
 	give.length=FALSE, give.attr=FALSE, indent.str="")
 	invisible(x)}

i.id <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["id"]]}

i.underlying <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["underlying"]]}

i.maturity <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["maturity"]]}

i.strike <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["strike"]]}

i.mode <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["mode"]]}

i.num <- function(x){
	if(!is(x,"security")) stop("wrong object.")
	x[["num"]]}

################# EQUITY ######################
###############################################
equity_s <- function(id, cur, num=1){
	maincl		<- "equity"
	sub			<- "spot"
	check_strings(id)
	cur 		<- check_currency(cur)
	temp 		<- list(id=id, underlying=id, cur=cur, num=num)
	class(temp) <- c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
    cat("<----", id, "---->\n")}


equity_f <- function(id, underlying, cur, maturity, num=1){
	maincl 		<- "equity"
	sub 		<- "future"
	check_strings(id, underlying)
	cur 		<- check_currency(cur)
	maturity	<- check_date(maturity)
	temp 		<- list(id=id, underlying=underlying, cur=cur, maturity=maturity, num=num)
	class(temp)	<-c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
	cat("<----", id, "---->\n")}

equity_o <- function(id, underlying, cur, maturity, strike, mode, type, class, num=1){
	# mode  	:0 - "call", 1 - "put"
	# type		:0 - "euro", 1 - "ame"
	# class		:"vanilla","power"
	maincl 		<- "equity"
	sub 		<- "option"
	check_strings(id, underlying, class)
	check_numerics(strike, mode, type)
	cur 		<- check_currency(cur)
	maturity	<- check_date(maturity)
	mode 		<- check_mode(mode)
	type 		<- check_type(type)
	class 		<- check_class(class)	
	temp 		<- list(id=id, underlying=underlying, cur=cur, maturity=maturity, 
											strike=strike, mode=mode, type=type, class=class, num=num)
	class(temp)<- c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
    cat("<----", id, "----->\n")}

################# COMMODITY ###################
###############################################
commod_s <- function(id, cur, num=1){
	maincl		<- "commod"
	sub			<- "spot"
	check_strings(id)
	cur 		<- check_currency(cur)
	temp 		<- list(id=id, cur=cur, num=num)
	class(temp) <- c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
    cat("<----", id, "---->\n")}


commod_f <- function(id, underlying, cur, maturity, num=1){
	maincl 		<- "commod"
	sub 		<- "future"
	check_strings(id, underlying)
	check_numerics(strike)
	cur 		<- check_currency(cur)
	maturity	<- check_date(maturity)
	temp 		<- list(id=id, underlying=underlying, cur=cur, maturity=maturity, num=num)
	class(temp)	<-c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
	cat("<----", id, "---->\n")}

commod_o <- function(id, underlying, cur, maturity, strike, mode, type, class, num=1){
	# mode  	:0 - "call", 1 - "put"
	# type		:0 - "euro", 1 - "ame"
	# class		:"vanilla","power"
	maincl 		<- "commod"
	sub 		<- "option"
	check_strings(id, underlying, class)
	check_numerics(strike, mode, type)
	cur 		<- check_currency(cur)
	maturity	<- check_date(maturity)
	mode 		<- check_mode(mode)
	type 		<- check_type(type)
	class 		<- check_class(class)	
	temp 		<- list(id=id, underlying=underlying, cur=cur, maturity=maturity, 
														strike=strike, mode=mode, type=type, class=class, num=num)
	class(temp)<- c("security", maincl, sub)
	assign(id,temp,envir = as.environment("securities"))
    cat("<----", id, "----->\n")}

################# FX ############################
#################################################
fx_s <- function(base, quote, num=1){
	maincl		<- "fx"
	sub			<- "spot"
	check_strings(base)
	quote 		<- check_currency(quote)
	temp 		<- list(base=base, quote=quote, num=num)
	class(temp) <- c("security", maincl, sub)
	assign(base,temp,envir = as.environment("securities"))
    cat("<----", base, "---->\n")}

fx_f <- function(id, base, quote, maturity, num=1){
	maincl 		<- "fx"
	sub 		<- "future"
	check_strings(id, base)
	check_numerics(strike)
	quote 		<- check_currency(quote)
	maturity	<- check_date(maturity)
	temp 		<- list(id=id, base=base, quote=quote, maturity=maturity, num=num)
	class(temp)	<-c("security", maincl, sub)
	assign(id, temp,envir=as.environment("securities"))
	cat("<----", id, "---->\n")}

fx_o <- function(id, base, quote, maturity, strike, mode, type, class, num=1){
	# mode  	:0 - "call", 1 - "put"
	# type		:0 - "euro", 1 - "ame"
	# class		:"vanilla","power"
	maincl 		<- "fx"
	sub 		<- "option"
	check_strings(id, base, class)
	check_numerics(strike, mode, type)
	base 		<- check_currency(base)
	quote 		<- check_currency(quote)
	maturity	<- check_date(maturity)
	mode 		<- check_mode(mode)
	type 		<- check_type(type)
	class 		<- check_class(class)	
	temp 		<- list(id=id, base=base, quote=quote, maturity=maturity, 
														strike=strike, mode=mode, type=type, class=class, num=num)
	class(temp)<- c("security", maincl, sub)
	assign(id,temp,envir=as.environment("securities"))
    cat("<----", id, "----->\n")}