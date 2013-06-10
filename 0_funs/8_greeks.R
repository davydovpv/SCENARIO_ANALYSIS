greeks_price <- function(ticker){
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			price <- s.s0(stock.par)
		}else if( is(instr,"future") ){
			rho 	<- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			price 	<- exp((g.r()-s.d(stock.par))*rho)*s.s0(stock.par)*0.05
		}else if( is(instr,"option") ){
			price 	<- price.eur.vanilla(0, s.s0(stock.par), stock.par, sec=instr)}
		price*i.num(instr)}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_delta <- function(ticker){
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			delta <- 1
		}else if( is(instr,"future") ){
			rho 	<- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			delta 	<- exp((g.r()-s.d(stock.par))*rho)
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			if( i.mode(instr)==0 ){ 	delta <- exp(-d*rho)*stats::pnorm( d_1,0,1)
			}else if( i.mode(instr)==1){delta <- -exp(-d*rho)*stats::pnorm(-d_1,0,1)}}
		delta*i.num(instr)}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_gamma <- function(ticker){
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			gamma 	<- 0
		}else if( is(instr,"future") ){
			gamma 	<- 0
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			gamma <- (1/sqrt(2*pi)*exp(-d_1^2/2)) / (s0*vol*sqrt(rho)) * exp(-d*rho)
		}
		gamma*i.num(instr)}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_theta <- function(ticker){
	# divided by 252! 1-day
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			theta 	<- 0
		}else if( is(instr,"future") ){
			theta 	<- 0
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			d_2 <- (log(s0/K)+((r-d)-vol^2/2)*rho)/(vol*sqrt(rho));
			if( i.mode(instr)==0 ){
				theta <- -s0*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-d*rho)/(2*sqrt(rho))+
                                    d*s0*stats::pnorm(d_1,0,1)*exp(-d*rho)-r*K*exp(-r*rho)*stats::pnorm(d_2,0,1)
			}else if( i.mode(instr)==1){
				theta <- -s0*(1/sqrt(2*pi)*exp(-d_1^2/2))*vol*exp(-d*rho)/(2*sqrt(rho))-
                                    d*s0*stats::pnorm(-d_1,0,1)*exp(-d*rho)+r*K*exp(-r*rho)*stats::pnorm(-d_2,0,1)}}
		theta*i.num(instr)/g.nyear()}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_rho <- function(ticker){
	# basis points!!!
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			gr_rho 	<- 0
		}else if( is(instr,"future") ){
			rho 	<- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			gr_rho 	<- rho * s.s0(stock.par) * exp((g.r()-s.d(stock.par))*rho)
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			d_2 <- (log(s0/K)+((r-d)-vol^2/2)*rho)/(vol*sqrt(rho));
			if( i.mode(instr)==0 ){
				gr_rho <- K*rho*exp(-r*rho)*stats::pnorm(d_2,0,1)
			}else if( i.mode(instr)==1){
				gr_rho <- -K*rho*exp(-r*rho)*stats::pnorm(-d_2,0,1)}}
		gr_rho*i.num(instr)/10000}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_rhoQ <- function(ticker){
	# basis points!!!
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			gr_rhoQ 	<- 0
		}else if( is(instr,"future") ){
			rho 	<- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			gr_rhoQ 	<- -rho * s.s0(stock.par) * exp((g.r()-s.d(stock.par))*rho)
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			d_2 <- (log(s0/K)+((r-d)-vol^2/2)*rho)/(vol*sqrt(rho));
			if( i.mode(instr)==0 ){
				gr_rhoQ <- -rho*exp(-d*rho)*s0*stats::pnorm(d_1,0,1)
			}else if( i.mode(instr)==1){
				gr_rhoQ <- rho*exp(-d*rho)*s0*stats::pnorm(-d_1,0,1)}}
		gr_rhoQ*i.num(instr)/10000}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_vega <- function(ticker){
	# 1% change
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]
	names(cur.instr)<- sapply(cur.instr,i.id)
	greek <- function(instr, stock.par){
		date.present 	<- s.date(stock.par)
		if ( is(instr,"spot") ){
			vega 	<- 0
		}else if( is(instr,"future") ){
			vega 	<- 0
		}else if( is(instr,"option") ){
			r 	<- g.r()
			d 	<- s.d(stock.par)
			rho <- time_diff_days(date.present, i.maturity(instr), cal=g.cal(), trading=TRUE) / g.nyear()
			s0 	<- s.s0(stock.par)
			vol <- s.vol(stock.par)
			K 	<- i.strike(instr)
			d_1 <- (log(s0/K)+((r-d)+vol^2/2)*rho)/(vol*sqrt(rho));
			d_2 <- (log(s0/K)+((r-d)-vol^2/2)*rho)/(vol*sqrt(rho));
			if( i.mode(instr)==0 ){
				vega <- s0*sqrt(rho)*(1/sqrt(2*pi)*exp(-d_1^2/2))*exp(-d*rho)
			}else if( i.mode(instr)==1){
				vega <- s0*sqrt(rho)*(1/sqrt(2*pi)*exp(-d_1^2/2))*exp(-d*rho)}}
		vega*i.num(instr)/100}
	sapply(cur.instr, greek, stock.par=stock.par)}

greeks_all <- function(ticker){
	prices 	<- greeks_price(ticker=ticker)
	deltas 	<- greeks_delta(ticker=ticker)
	gammas 	<- greeks_gamma(ticker=ticker)
	thetas 	<- greeks_theta(ticker=ticker)
	vegas 	<- greeks_vega(ticker=ticker)
	rhos 	<- greeks_rho(ticker=ticker)
	rhosQ 	<- greeks_rhoQ(ticker=ticker)
	temp <- rbind(prices,deltas,gammas,thetas,vegas,rhos,rhosQ)
	rownames(temp)=c("prices", "deltas", "gammas", "thetas/tdays", "vegas/100", "rhos/10000", "rhosQ/10000")
	temp}


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