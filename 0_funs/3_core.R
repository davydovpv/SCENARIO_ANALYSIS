payoff.aggregate 	<- function(sec, tVec, stock.par, cur.range){
	if ( is(sec,"spot") ){
		payoff 	<- sapply(tVec, price.equity, cur.range=cur.range, stock.par=stock.par, sec=sec)
	}else if( is(sec,"future") ){
		payoff 	<- sapply(tVec, price.equity, cur.range=cur.range, stock.par=stock.par, sec=sec)
	}else if( is(sec, "option") ){
		payoff 	<- sapply(tVec, price.eur.vanilla, cur.range=cur.range, stock.par=stock.par, sec=sec)
	}
	payoff*i.num(sec)}

cost.aggregate 	<- function(sec, stock.par){
	if ( is(sec,"spot") ){
		cost <- price.equity(0, s.s0(stock.par), stock.par=stock.par, sec=sec)
	}else if( is(sec,"future") ){
		cost <- price.future(0, s.s0(stock.par), stock.par=stock.par, sec=sec)
	}else if( is(sec, "option") ){
		cost <- price.eur.vanilla(0, s.s0(stock.par), stock.par=stock.par, sec=sec)}
	cost*i.num(sec)}

