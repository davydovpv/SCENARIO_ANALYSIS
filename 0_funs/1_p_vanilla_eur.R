# BSM_gen <- function(S0=100, K=100, r=0, b=0, T=0.0001, vol=0.1, opt=1){
#     # BSM_gen(S0=100, K=100, r=0.05, b=0.05, T=1, vol=0.1)
#     d_1  <- (log(S0/K)+(b+vol^2/2)*T)/(vol*sqrt(T));
#     d_2  <- d_1 - vol*sqrt(T)             # d_2  =(log(S0/K)+(b-vol^2/2)*T)/(vol*sqrt(T));
#     if(opt==1){          S0*exp((b-r)*T)*stats::pnorm( d_1,0,1)-K*exp(-r*T)*stats::pnorm( d_2,0,1)
#     }else if(opt==0){    -S0*exp((b-r)*T)*stats::pnorm(-d_1,0,1)+K*exp(-r*T)*stats::pnorm(-d_2,0,1)
#     }else{noquote("CALLS.... AND PUTS!")}}

price.eur.vanilla <- function(t, cur.range, stock.par, sec){
	T 		<- time_diff_days(s.date(stock.par), i.maturity(sec), cal=g.cal())/g.nyear()
	tau 	<- T-t
    K       <- i.strike(sec);
    r 		<- g.r();
    vol 	<- s.vol(stock.par);
    F0 		<- cur.range*exp( (r-s.d(stock.par))*tau );
    d_1  	<- (log(F0/K)+vol^2/2*tau)/(vol*sqrt(tau));
    d_2     <- d_1 - vol*sqrt(tau);
    switch(as.character(i.mode(sec)),
        "0" = exp(-r*tau)*(  F0*stats::pnorm( d_1,0,1) - K*stats::pnorm( d_2,0,1) ),
        "1" = exp(-r*tau)*( -F0*stats::pnorm(-d_1,0,1) + K*stats::pnorm(-d_2,0,1) ), "opt") }

