
############################ STRATEGIES FUNCTIONS #######################################
#########################################################################################
main.aggregate <- function(ticker, nsd=3, step=0.001, nstd=1.96, plot=FALSE, add=rep(0,length(securities))){
	# gen.par 		: vaGenPar
	# stock.par 	: vaSpar; vaStockPar
	# instruments 	: vaInstruments
	# nsd 			: number of standard deviations for stock
	securities 	<- vaSecurities()
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	cur.instr 	<- securities[which(sapply(securities,i.underlying)==ticker)]

	######################## TIMES ########################################
	date.present 	<- s.date(stock.par)
	date.start 		<- g.date.start()
	date.end 		<- g.date.end()
    tVec.st 		<- time_diff_days(date.present, date.start, cal=g.cal(), trading=TRUE)
    tVec.end 		<- time_diff_days(date.present, date.end, cal=g.cal(), trading=TRUE)
	tVec 			<- (tVec.st:tVec.end) / g.nyear()

	######################## MAIN LOOP FOR EACH TICKER ####################
	names(cur.instr)<- sapply(cur.instr,i.id)
	cur.bm		<- nsd*s.vol(stock.par)*sqrt(tail(tVec,1))
	cur.abm 	<- (g.r()-s.vol(stock.par)^2/2) + seq(-cur.bm, cur.bm, step)
	cur.range 	<- s.s0(stock.par)*exp(cur.abm)
	payoffs 	<- lapply(cur.instr, payoff.aggregate, tVec=tVec, stock.par=stock.par, cur.range=cur.range)
    costs   	<- sapply(cur.instr, cost.aggregate, stock.par=stock.par)
    pnl 		<- mapply(function(x, y){x-y}, x=payoffs, y=costs, SIMPLIFY=FALSE)
    pnl.gen 	<- Reduce('+', pnl)

    ######################## STANDARD DEVIATION ###########################
    std.bm		<- nstd*s.vol(stock.par)*sqrt(tVec)
    std.abm 	<- (g.r()-s.vol(stock.par)^2/2) + cbind(std.bm, -std.bm)
    std.gbm 	<- s.s0(stock.par)*exp(std.abm)
	std.fun <- function(x, range){
		n 		<- length(range)
		temp 	<- rep(NaN, n)
		for(i in x){
			j 	<- which.min(abs(range - i))
			if(j > (n-1)) j=n-1
			temp[(j-1):(j+1)]<-0}
			if(length(temp)!=length(range)) cat(x, " ")
		temp}
	std.mat 	<- apply(std.gbm, 1, std.fun, range=cur.range)

	######################## PLOTTING FUNCTIONS ###########################
    if (plot){
    	x       	<- cur.range	#1:nrow(pnl.gen)
	    y       	<- tVec
	    z 			<- pnl.gen
	    open3d(); bg3d("slategray"); material3d(col="black")
	    persp3d(x, y, z, col=rainbow(100)[cut(z, 100)], alpha=1, back="lines", axes = TRUE, box = TRUE,front="lines",xlab="", ylab="", zlab="")
	    persp3d(x, y, matrix(0,length(x),length(y)), col = "grey", alpha=1, back="points",front="points", add=TRUE, size=0.05)
	    persp3d(x, y, std.mat, col = "red", alpha=1, back="lines",front="lines", add=TRUE, size=0.05)
	    j 	<- 1
	    for(i in pnl){
	    	if(add[j]==1){
	    		persp3d(x, y, i, col = "white", alpha=1, back="points",front="points", add=TRUE, size=0.05, aspect="iso")}
	    	j 	<- j+1
	    }
    }
	######################## OUTPUT #######################################
    FINALFUN <- function(date, stock){
	    t.val <<- time_diff_days(date.present, date, cal=g.cal(), trading=TRUE)/g.nyear()
		obj=list(x=cur.range, y=tVec, z=pnl.gen)
		loc=cbind(stock,t.val)
		interp.surface(obj,loc)}
	return(FINALFUN)
}

find.extrema <- function(output, ticker, date, n=2){
	stock.pars 	<- vaStockPars()
	stock.par 	<- stock.pars[[which(sapply(stock.pars,s.ticker)==ticker)]]
	x 	<- seq(s.s0(stock.par)*0.7, s.s0(stock.par)*1.3, 0.001)
	y 	<- output(date, x)
	c(x[match(sort(abs(y))[1:n], abs(y))],x[c(which.min(y),which.max(y))])}



