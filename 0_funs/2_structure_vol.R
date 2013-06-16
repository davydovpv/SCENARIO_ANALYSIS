vaBS <- function(S0, K, r, d, tau, vol, put=FALSE){
	F0 	<- S0*exp((r-d)*tau)
    d_2 <- (d_1 <- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau))) - vol*sqrt(tau)
    if(!put){exp(-r*tau)*(  F0*pnorm( d_1) - K*pnorm( d_2) )
	}else{	 exp(-r*tau)*( -F0*pnorm(-d_1) + K*pnorm(-d_2) ) } }


# vaBS(80, 100, 0.05, 0.02, 1, 0.3, put=0)

eur_vanilla_iv <- function(P, S0, K, r, d, tau, put=FALSE){
    FUN1 <- function(vol, K, tau){
    	F0 	<- S0*exp((r-d)*tau)
	    d_2 <- (d_1 <- (log(F0/K) + vol^2/2*tau)/(vol*sqrt(tau))) - vol*sqrt(tau)
	    if(!put){exp(-r*tau)*(  F0*pnorm( d_1) - K*pnorm( d_2) ) - P
		}else{	 exp(-r*tau)*( -F0*pnorm(-d_1) + K*pnorm(-d_2) ) - P } }
    uniroot(FUN1, K=K, tau=tau, interval=c(0, 4))$root}

# system.time(eur_vanilla_iv(13.02028, 100, 100, 0.05,0.02,1))