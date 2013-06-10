price.equity <- function(t, cur.range, stock.par, sec){cur.range}

price.future <- function(t, cur.range, stock.par, sec){
	T 	<- time_diff_days(s.date(stock.par), i.maturity(sec), cal=cal)/g.nyear()
	rho <- T-t
    cur.range * exp(r*rho)}