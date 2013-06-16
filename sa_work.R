source("~/core/Projects/scenario_analysis/0_sa_init.R")
########################################################################
GenPar(
	rates = list("USD"=0.05),
	nyear = 252
	)

StockPar(
	ticker	= "aapl", 		# ticker of the stock
	s0		= 100, 			# current price of the stock given time(date above)
	vol		= 0.3, 			# yearly vol(numeric)
	d 		= 0, 			# 
	date 	= Sys.Date()	# "timeDate"
	)

########################################################################
# Spot("AAPL")
# Forward("AAPLF", "AAPL", Sys.Date()+days(30), 110)
# Option("AAPLC", "AAPL", Sys.Date()+days(30), 100, 0, 0, type="binary", num=1)

Spot("AAPL", num= -1)
Option("AAPLC", "aapl", Sys.Date()+days(30), 120, 0, 0, type="vanilla", num= 1)
Option("AAPLP", "aapl", Sys.Date()+days(30), 80 , 1, 0, type="vanilla", num=-1)
Option("AAPLB", "aapl", Sys.Date()+days(30), 110 , 0, 0, type="binary", num=30)

t 	<- time_seq(Sys.Date(),Sys.Date()+2)
s 	<- seq(80,120,10)
v 	<- seq(0.1,0.5,0.1)

AAPLC$price() 				# current price [1,] 0.05804536
AAPLC$delta() 				# current delta
AAPLC$getiv(AAPLC$price())	# [1] 0.3000074

# say you observe the price higher in the market: 0.1!
AAPLC$getiv(0.1)			# [1] 0.3263369
# assign it to stock parameter!
AAPL.par@vol <- AAPLC$getiv(0.1) 	# you have correct volatility now


p1 = AAPLC$price(st=s, tVec=t) 		# use st with tVec
p2 = AAPLC$price(vol=v, tVec=t) 	# or vol with tVec
p3 = AAPLC$price(vol=v, st=s)		# DON'T use st with vol
str(p1)
# Formal class 'scatter' [package ".GlobalEnv"] with 4 slots
#   ..@ .Data: num [1:5, 1:8] 1.63e-06 8.97e-04 5.80e-02 8.34e-01 4.28 ...
#   ..@ rows : num [1:5] 80 90 100 110 120
#   ..@ rows2: num 0.3
#   ..@ cols : num [1:8] 0.0794 0.0789 0.0784 0.0779 0.0774 ...

# the output is the same("scatter") from all methods except "getiv"
class(p1)		# scatter
plot(p1)
class(p1+20) 	# scatter

b1 = AAPLP$price(st=s, tVec=t)
# sum the payoffs
class(p1+b1) 		# matrix - not good
plot(p1+b1)			# not a good idea

class(sum(p1, b1))	# scatter - stick to this
plot(sum(p1, b1))
# sum creates new "scatter" object 
# and all attributes are taken from 1st object!
# MEANS: never put a "spot" object first as due to the fact, 
# 		that it does not have maturity, time axis will be incorrect!

cost <- as.numeric(sum(AAPLC$price(), AAPLP$price(), AAPL$price(), AAPLB$price()))
cost 	# [1] -96.04445 means you have cash!
port <- sum(AAPLC$price(s,t), AAPLP$price(s,t), AAPL$price(s,t), AAPLB$price(s,t))

plot(port-cost) 	# here is your proffit and loss!
vega <- sum(AAPLC$vega(s,t), AAPLP$vega(s,t), AAPL$vega(s,t), AAPLB$vega(s,t))
plot(port-cost)		# and here is your vega


# make parameters longer and finer to generate nice graphs:
t 	<- time_seq(Sys.Date(),Sys.Date()+30)
s 	<- seq(80,120,1)
# and repeat



plot(DIGI$delta(s,t))
plot(DIGI$gamma(s,t))
plot(DIGI$theta(s,t))
plot(DIGI$rho(s,t))
plot(DIGI$rhoQ(s,t))
plot(DIGI$vega(s,t))
