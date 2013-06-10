vaBS <- function(S0, K, r, d, tau, vol, opt=0){
    d_1  <- (log(S0/K)+((r-d)+vol^2/2)*tau)/(vol*sqrt(tau));
    d_2  <- d_1 - vol*sqrt(tau) # d_2  =(log(S0/K)+(b-vol^2/2)*tau)/(vol*sqrt(tau));
    switch(as.character(opt),          
        "0" =  S0*exp(-d*tau)*stats::pnorm( d_1,0,1) - K*exp(-r*tau)*stats::pnorm( d_2,0,1),
        "1" = -S0*exp(-d*tau)*stats::pnorm(-d_1,0,1) + K*exp(-r*tau)*stats::pnorm(-d_2,0,1), "opt")}

european_vanilla_iv <- function(price, FUN=vaBS, S0, K, r, d, tau, opt=1){
    inter <- c(0, 4)
    FUN1 <- function(vol, ...){FUN(vol, ...)-price}
    uniroot(FUN1, opt=opt, S0=S0, K=K, r=r, d=d, tau=tau, interval=inter)$root}

r <- log(1+0.07751)
S0 = 29.52
K = 27.24
P = 2.86
d = 0
tau = 26/252

european_vanilla_iv(P, FUN=vaBS, S0, K, r, d=0, tau, 0)

saveRDS(close,"~/desktop/andre.xts")

data <- readRDS("~/desktop/andre.xts")
head(close)
ret <- ROC(data, na.pad=FALSE)
head(ret)

chartSeries(data)


r <- log(1+0.07751)
S0 = 29.52
K = 27.24
P = 2.86
d = 0
tau = 26/252
vol = 0.3007

for(i in 1:1000){

}
sapply(1:1000,function(){sum(sample(recent,26))})

recent <- ret["201206/"]
hist(ret,breaks=50)

chart.Histogram(ret, breaks=80, probability=TRUE, main="", methods=c("add.qqplot","add.normal"))
chart.Histogram(recent, breaks=80, probability=TRUE, main="", methods=c("add.qqplot","add.normal"))




