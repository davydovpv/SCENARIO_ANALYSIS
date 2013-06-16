############################# LIST #######################################################
##########################################################################################
list.rm.null  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]}

normalit <- function(mmat, st, end){
    onezero <- (mmat - min(mmat))/(max(mmat)-min(mmat))
    st + onezero*(end-st)}

############################# TIME #######################################################
##########################################################################################
time_seq <- function(from, to, by="hour", rng=c(9,16), holiday=holidayNYSE(2013:2020), trading=TRUE){
    # by            * A number, taken to be in seconds.
    #               * A object of class 'difftime'
    #               * "sec", "min", "hour", "day", "DSTday", "week", "month", "year"
    from    <- as.timeDate(from)
    to      <- as.timeDate(to)
    temp    <- seq(from=from, to=to, by=by)
    temp    <- temp[get(by)(temp)>=rng[1] & get(by)(temp)<=rng[2]]
    if(trading){ temp <- temp[isBizday(temp, holiday)]}
    return(temp)}

time_diff <- function(from, to, by="hour", rng=c(9,16), holiday=holidayNYSE(2013:2020), trading=TRUE){
    from    <- as.timeDate(from)
    to      <- as.timeDate(to)
    if(length(from)>1 && length(to)==1){
        temp    <- seq(from=from[1], to=to, by=by)
        temp    <- temp[get(by)(temp)>=rng[1] & get(by)(temp)<=rng[2]]
        if(trading){ temp <- temp[isBizday(temp, holiday)]}
        sapply(from,function(x){length(cut(temp, from=x, to=to))})
    }else if(length(from)==1 && length(to)>1){
        temp    <- seq(from=from, to=to[length(to)], by=by)
        temp    <- temp[get(by)(temp)>=rng[1] & get(by)(temp)<=rng[2]]
        if(trading){ temp <- temp[isBizday(temp, holiday)]}
        sapply(to,function(x){length(cut(temp, from=from, to=x))})
    }else if(length(from)==1 && length(to)==1){
        temp    <- seq(from=from, to=to, by=by)
        temp    <- temp[get(by)(temp)>=rng[1] & get(by)(temp)<=rng[2]]
        if(trading){ temp <- temp[isBizday(temp, holiday)]}
        length(temp)
    } }

############################# PLOT #######################################################
##########################################################################################
my3d_plot <- function(mat, xl="", yl="", zl="", FUN=persp3d){
    x       <- 1:nrow(mat)
    y       <- 1:ncol(mat)
    colorlut<- heat.colors(ncol(mat),alpha=0)
    col     <- rev(rep(colorlut,each=nrow(mat)))
    open3d(); bg3d("slategray"); material3d(col="black")
    FUN(x, y, mat, col = col, alpha=1, back="points", axes = TRUE, box = TRUE,front="lines",xlab=xl, ylab=yl, zlab=zl)}



