source("~/core/Projects/scenario_analysis/0_sa_init.R")

######################## GEN AND STOCK ################################
vaGenPar(date.start=Sys.Date(), date.end=as.Date("2013-07-15"), r=log(1+0.07751), cal="UnitedStates")
vaStockPar("AAPL", s0=29.09, date=Sys.Date(), vol=0.3007)

######################## PORTFOLIO ####################################
rmi()
# equity_s("AAPL","USD", num=-1)
# equity_f("AAPL_fut",  "AAPL", "USD", Sys.Date()+days(180), num=1)
# equity_o("AAPL1", "AAPL", "USD", Sys.Date()+days(360), 80, 0, 0, "vanilla", num= 1)
# equity_o("AAPL2", "AAPL", "USD", Sys.Date()+days(360), 100, 0, 0, "vanilla", num= -2)
# equity_o("AAPL3", "AAPL", "USD", Sys.Date()+days(360), 120, 0, 0, "vanilla", num= 1)
equity_o("AAPL1", "AAPL", "USD", as.Date("2013-07-15"), 29.09, 1, 0, "vanilla", num= 1)
equity_o("AAPL2", "AAPL", "USD", as.Date("2013-07-15"), 29.09, 0, 0, "vanilla", num= 1)

######################## HEDGING #########################
greeks <- greeks_all("AAPL"); greeks
equity_s("AAPL","USD", num=-apply(greeks,1,sum)["deltas"]) 	# delta hedging



######################## RUN ###########################################
butter <- main.aggregate(ticker="AAPL", nsd=3, plot=TRUE, step=0.005)



######################## ANALYSE #######################################
rgl.close()
x 	<- seq(s.s0(AAPL.par)*0.7, s.s0(AAPL.par)*1.3, 0.001)
y 	<- butter(Sys.Date()+days(1), x)

ex 	<- find.extrema(butter, "AAPL", Sys.Date()+days(1))
plot(x,y,"l", col="blue",xlim=c(min(x)+7,max(x)-2)); abline(h=0); abline(v=c(s.s0(AAPL.par),ex))









# play3d(spin3d(axis=c(0,0,1), rpm=5), duration=12)
# movie3d(spin3d(axis=c(0,0,1), rpm=5), duration=12)
# rgl.postscript("example.pdf","pdf")
# rgl.snapshot("example2.png")