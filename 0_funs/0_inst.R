################# BASIC CLASSES ##########################################################
##########################################################################################
setClass("currency", representation(string = "character"), contains = "character")
Currency <- function(string){
	if( nchar(string <- toupper(string)) != 3 ){stop("wrong currency")}
	new("currency", string)}


#### SCATTER ###########################################
setClass("scatter", contains = "matrix", representation(
	rows = "numeric", 		# stock
	rows2 = "numeric",		# vol
	cols = "numeric" 		# tau
	))

setMethod("plot", "scatter", function(x, y=FALSE, zero=FALSE) {
    y       	<- 1:length(x@cols)
    z 			<- x@.Data
    if(nrow(z)==length(x@rows)) x <- x@rows else x <- x@rows2
    open3d(); bg3d("slategray"); material3d(col="black")
    persp3d(x, y, z, col=rainbow(100)[cut(z, 100)], alpha=1, back="lines", axes = TRUE, box = TRUE,front="lines",xlab="", ylab="", zlab="")
    if(zero){
    	persp3d(x, y, matrix(0,length(x),length(y)), col = "grey", alpha=1, back="points",front="points", add=TRUE, size=0.05)
    }
})

setMethod("show", "scatter", function(object) {print(object@.Data)})

setMethod("sum","scatter",function(x,...){
	rows <- x@rows
	cols <- x@cols
	new("scatter", data=Reduce('+',list(...),x), rows=rows, cols=cols)
})


################# GEN_PAR ################################################################
##########################################################################################
attach(what=NULL,name="gen.par")
lsg <- function(){get("gen.par", envir=as.environment("gen.par"))}
setClass("gen.par", representation(
	rates 		= "list",
	nyear 		= "numeric" ))
setMethod("show", "gen.par", function(object) {str(object)})

GenPar <- function(rates, nyear=252){
	rm( list=ls("gen.par"), envir=as.environment("gen.par") )
	temp <- new("gen.par", rates=rates, nyear=nyear)
	assign("gen.par",temp,envir=as.environment("gen.par"))
    cat("<---- gen.par ---->\n")}


################# STOCK_PAR ##############################################################
##########################################################################################
attach(what=NULL,name="stock.par")
lss <- function(){ls("stock.par")}
rms <- function(){rm(list=ls("stock.par"),envir=as.environment("stock.par"))}
setClass("stock.par", representation(
	ticker 	= "character",
	s0 		= "numeric",
	vol 	= "numeric",
	d 		= "list",
	date 	= "timeDate" ))
setMethod("show", "stock.par", function(object) {str(object)})

StockPar 	<- function(ticker, s0, vol, d=0, date){
	ticker <- toupper(ticker)
	temp <- new("stock.par", ticker=ticker, s0=s0, vol=vol, d=as.list(d), date=as.timeDate(date))
	assign(paste0(ticker,".par"),temp,envir=as.environment("stock.par"))
    cat("<---- ", ticker,".par ---->\n",sep="")}



################# SECURITIES #############################################################
##########################################################################################
attach(what=NULL,name="securities")
lsi <- function(){ ls("securities") }
rmi <- function(){ rm(list=ls("securities"), envir=as.environment("securities")) }
vaSecurities <- function(){
	objects=ls("securities"); temp <- list()
	for(i in objects){temp[[length(temp)+1]]=get(i,envir=as.environment("securities"))}
	temp}

Security <- setRefClass("security",
	fields = list(
		id 			= "character",
		num 		= "numeric",
		class 		= "character",
		cur 		= "currency"
		),
		methods = list(
			show = function(){
				str(.self)
			} ) )

#### Spot #############################

Spot <- function(id, num=1, class="equity", cur="usd"){
	id <- toupper(id)
	if(!exists("gen.par", envir=as.environment("gen.par"))){stop("Define GenPar")}
	if(!exists(paste0(id,".par"), envir=as.environment("stock.par"))){stop("Define StockPar")}
	temp <- SpotClass(id=id, num=num, class=class, cur=Currency(cur))
	assign(id,temp,envir = as.environment("securities"))
	cat("<----", id, "---->\n")}

SpotClass <- setRefClass("spot", contains="security",
	fields = list(
		underlying	= "character"
	 	),
	methods = list( 
	 	initialize = function(id=id, ...) {
	 		underlying 	<<- id
			callSuper(id=id, ...)
		},
		price = function(st, tVec, vol){
	 		par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- replicate(length(tVec), st * num)
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		delta = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(1, length(st), length(tVec)) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		gamma = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		theta = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		rho = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		rhoQ = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		vega = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- 1:length(tVec)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		} ) )


#### Forward #############################
Forward <- function(id, underlying, maturity, K, num=1, class="equity", cur="usd"){
	id <- toupper(id); underlying=toupper(underlying)
	temp <- ForwardClass(id=id, underlying=underlying, maturity=as.timeDate(maturity), K=K, num=num, class=class, cur=Currency(cur))
	assign(id,temp,envir = as.environment("securities"))
	cat("<----", id, "---->\n")}

ForwardClass <- setRefClass("forward", contains="security",
	fields = list(
		underlying	= "character",
		maturity 	= "timeDate",
		K 			= "numeric"
	 	),
	methods = list( 
	 	initialize = function(...) {
			callSuper(...)
		},
		price = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- sapply(tau, function(tau){st - K*exp(-(r-d)*tau)}) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		delta = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- matrix(1, length(st), length(tVec)) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		gamma = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		theta = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- sapply(tau, function(tau){ rep(-(r-d)*K*exp(-(r-d)*tau), length(st) ) }) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		rho = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- sapply(tau, function(tau){ rep(tau*K*exp(-(r-d)*tau), length(st) ) }) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		rhoQ = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp <- sapply(tau, function(tau){ rep(-tau*K*exp(-(r-d)*tau), length(st) ) }) * num
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		},
		vega = function(st, tVec, vol){
			par 	<- get(paste0(id,".par"), envir=as.environment("stock.par"))
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			temp 	<- matrix(0, length(st), length(tVec))
			new("scatter", data=temp, rows=st, rows2=vol, cols=tau)
		} ) )


#### Option #############################
Option <- function(id, underlying, maturity, K, put, ame=0, type="vanilla", num=1, extra=list(0), class="equity", cur="usd"){
	id <- toupper(id); underlying=toupper(underlying)
	temp <- OptionClass(id=id, underlying=underlying, maturity=as.timeDate(maturity), K=K, 
		put=as.logical(put), ame=as.logical(ame), type=tolower(type), num=num, extra=extra, class=class, cur=Currency(cur))
	assign(id,temp,envir = as.environment("securities"))
	cat("<----", id, "---->\n")}

OptionClass <- setRefClass("option", contains="security",
	fields = list(
		underlying	= "character",
		maturity 	= "timeDate",
		K 			= "numeric",
		put 		= "logical",
		ame 		= "logical",
		type 		= "character",
		extra 		= "list"
	 	),
	methods = list( 
	 	initialize = function(...) {
			callSuper(...)
		},
		getiv = function(P){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			tau 	<- time_diff(par@date, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					FUN1 <- function(vol){
				    	F0 	<- par@s0*exp((r-d)*tau)
					    d_2 <- (d_1 <- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau))) - vol*sqrt(tau)
					    if(!put){exp(-r*tau)*(  F0*stats::pnorm( d_1) - K*stats::pnorm( d_2) ) - P
					    }else{	 exp(-r*tau)*( -F0*stats::pnorm(-d_1) + K*stats::pnorm(-d_2) ) - P } }
					    uniroot(FUN1, interval=c(0, 4))$root
				}else if(type == "binary"){
					FUN1 <- function(vol){
				    	F0 	<- par@s0*exp((r-d)*tau)
					    d_2 <- (d_1 <- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau))) - vol*sqrt(tau)
					    if(!put){exp(-r*tau)*stats::pnorm( d_2) - P
					    }else{	 exp(-r*tau)*stats::pnorm(-d_2) - P } }
					    uniroot(FUN1, interval=c(0, 4))$root
				}
			}else{ NA }
		},
		price = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					price <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){ exp(-r*tau)*(  F0*stats::pnorm( d_1) - K*stats::pnorm( d_2,0,1) ) 
						}else	{ exp(-r*tau)*( -F0*stats::pnorm(-d_1) + K*stats::pnorm(-d_2,0,1) )} })
				}else if(type == "binary"){
					price <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_2 	<- (log(F0/K) - vol^2/2*tau)/(vol*sqrt(tau));
						if(!put){ exp(-r*tau) * stats::pnorm(  d_2) 
						}else 	{ exp(-r*tau) * stats::pnorm( -d_2) } })
				}
				new("scatter", data=price * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		delta = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					delta <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						if(!put){ exp(-d*tau)*stats::pnorm( d_1)
						}else{   -exp(-d*tau)*stats::pnorm(-d_1) } })
				}else if(type == "binary"){
					delta <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_2 	<- (log(F0/K) - vol^2/2*tau)/(vol*sqrt(tau));
						if(!put){ exp(-r*tau)*stats::dnorm( d_2)/( vol*st*sqrt(tau) )
						}else{   -exp(-r*tau)*stats::dnorm(-d_2)/( vol*st*sqrt(tau) ) } })
				}
				new("scatter", data=delta * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		gamma = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					gamma <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						(1/sqrt(2*pi)*exp(-d_1^2/2)) / (st*vol*sqrt(tau)) * exp(-d*tau) })
				}else if(type == "binary"){
					gamma <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){ -exp(-r*tau) * d_1 * stats::dnorm(d_2) / (vol*st*tau)^2
						}else{     exp(-r*tau) * d_1 * stats::dnorm(d_2) / (vol*st*tau)^2 } })
				}
				new("scatter", data=gamma * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		theta = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if( missing(st) ){st=par@s0}
			if( missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					theta <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){ -st*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-d*tau)/(2*sqrt(tau))+
	                                    d*st*stats::pnorm( d_1)*exp(-d*tau)-r*K*exp(-r*tau)*stats::pnorm( d_2)
						}else{ 	  -st*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-d*tau)/(2*sqrt(tau))-
	                                    d*st*stats::pnorm(-d_1)*exp(-d*tau)+r*K*exp(-r*tau)*stats::pnorm(-d_2) } } )
				}else if(type == "binary"){
					theta <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){ r*exp(-r*tau)*stats::pnorm( d_2)+exp(-r*tau)*stats::dnorm(d_2)*(d_1/(2*tau)-(r-d)/(vol*sqrt(tau)))
						}else{ 	  r*exp(-r*tau)*stats::pnorm(-d_2)-exp(-r*tau)*stats::dnorm(d_2)*(d_1/(2*tau)-(r-d)/(vol*sqrt(tau))) } })
				}
				new("scatter", data=theta * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		rho = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					rho <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){  K*tau*exp(-r*tau)*stats::pnorm( d_2)
						}else{    -K*tau*exp(-r*tau)*stats::pnorm(-d_2) } })
				}else if(type == "binary"){
					rho <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_2     <- (log(F0/K) - vol^2/2*tau)/(vol*sqrt(tau));
						if(!put){ -tau * exp(-r*tau)*stats::pnorm( d_2) + tau/vol * exp(-r*tau)*stats::dnorm(d_2)
						}else{ 	  -tau * exp(-r*tau)*stats::pnorm(-d_2) - tau/vol * exp(-r*tau)*stats::dnorm(d_2)} })
				}
				new("scatter", data=rho * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		rhoQ = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					rhoQ <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						d_2     <- d_1 - vol*sqrt(tau);
						if(!put){ -tau*exp(-d*tau)*st*stats::pnorm( d_1)
						}else{ 	   tau*exp(-d*tau)*st*stats::pnorm(-d_1) } })
				}else if(type == "binary"){
					rhoQ <- matrix( NA, length(st), length(tVec))
				}
				new("scatter", data=rhoQ * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		},
		vega = function(st, tVec, vol){
	 		par 	<- get(paste0(underlying,".par"), envir=as.environment("stock.par"))	 		
			gpar 	<- get("gen.par", envir=as.environment("gen.par"))
			if(missing(st)){st=par@s0}
			if(missing(vol)){vol=par@vol}
			if(missing(tVec)){tVec=par@date}
			tau 	<- time_diff(tVec, maturity)/(gpar@nyear*8)
			d 		<- par@d[[1]]
			r 		<- gpar@rates[[cur]]
			if( !ame ){
				if(type == "vanilla"){
					vega <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_1  	<- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau));
						st*sqrt(tau)*(1/sqrt(2*pi)*exp(-d_1^2/2))*exp(-d*tau) })
				}else if(type == "binary"){
					vega <- sapply(tau, function(tau){
						F0 		<- st*exp((r-d)*tau)
						d_2     <- (log(F0/K) - vol^2/2*tau)/(vol*sqrt(tau));
						if(!put){ -exp(-r*tau) * stats::dnorm(d_2)*(sqrt(tau)+d_2/vol)
						}else{ 	   exp(-r*tau) * stats::dnorm(d_2)*(sqrt(tau)+d_2/vol)} })
				}
				new("scatter", data=vega * num, rows=st, rows2=vol, cols=tau)
			}else{ NA }
		} ) )


# vanilla_european <- function(S0, K, r, b, T, vol, opt){
#     d_1  <- (log(S0/K)+(b+vol^2/2)*T)/(vol*sqrt(T));
#     d_2  <- (log(S0/K)+(b-vol^2/2)*T)/(vol*sqrt(T));
#     if(opt==0){
#         value   <-  S0*exp(-(b-r)*T)*stats::pnorm( d_1,0,1)-K*exp(-r*T)*stats::pnorm(d_2,0,1)
#         delta   <-  exp(-(b-r)*T)*stats::pnorm( d_1,0,1)
#         gamma   <- (1/sqrt(2*pi)*exp(-d_1^2/2))/(S0*vol*sqrt(T))*exp(-(b-r)*T)
#         vega    <- S0*sqrt(T)*(1/sqrt(2*pi)*exp(-d_1^2/2))*exp(-(b-r)*T)
#         vomma   <- vega*d_1*d_2/vol
#         theta   <- -S0*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-d*T)/(2*sqrt(T))+
#                                     (b-r)*S0*stats::pnorm(d_1,0,1)*exp(-(b-r)*T)-r*K*exp(-r*T)*stats::pnorm(d_2,0,1)
#         rho     <- K*T*exp(-r*T)*stats::pnorm(d_2,0,1)
#         rhoQ    <- -T*exp(-(b-r)*T)*S0*stats::pnorm(d_1,0,1)
#         lambda  <- delta*S0/value
#     }else if(opt==1){
#         value   <- -S0*exp(-(b-r)*T)*stats::pnorm(-d_1,0,1)+K*exp(-r*T)*stats::pnorm(-d_2,0,1)
#         delta   <- -exp(-(b-r)*T)*stats::pnorm(-d_1,0,1)
#         gamma   <- (1/sqrt(2*pi)*exp(-d_1^2/2))/(S0*vol*sqrt(T))*exp(-(b-r)*T)
#         vega    <- S0*sqrt(T)*(1/sqrt(2*pi)*exp(-d_1^2/2))*exp(-(b-r)*T)
#         vomma   <- vega*d_1*d_2/vol
#         theta   <- -S0*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-(b-r)*T)/(2*sqrt(T))-
#                                     (b-r)*S0*stats::pnorm(-d_1,0,1)*exp(-(b-r)*T)+r*K*exp(-r*T)*stats::pnorm(-d_2,0,1)
#         rho     <- -K*T*exp(-r*T)*stats::pnorm(-d_2,0,1)
#         rhoQ    <- T*exp(-(b-r)*T)*S0*stats::pnorm(-d_1,0,1)
#         lambda  <- delta*S0/value
#     }else{
#         stop("opt=0 means CALL; opt=1 means PUT")
#     }
#     return(cbind(value,delta,lambda,gamma,vega,vomma,theta,rho,rhoQ))
# }