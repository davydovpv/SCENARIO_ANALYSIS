############################# LIST #######################################################
##########################################################################################
list.rm.null  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]}

############################# MATRIX #####################################################
##########################################################################################
normalit <- function(mmat, st, end){
    onezero <- (mmat - min(mmat))/(max(mmat)-min(mmat))
    st + onezero*(end-st)}


############################# TIME #######################################################
##########################################################################################
time_match <- function(time){
    if (is(time, "character") || is(time, "numeric")){
        time <- as.POSIXct(as.character(time),format=timeDate::whichFormat(time))}
    if (is(time, "timeSeries") || is(time, "xts") || is(time, "zoo")){
        time=timeSeries::time(time)}
    if (is(time, "timeDate") || is(time, "POSIXt") || is(time, "Date")) 
        time <- as.character(time)
    time}

time_seq <- function(from=Sys.time(), to, by="day", cal = "UnitedStates", trading=TRUE){
    # by            * A number, taken to be in seconds.
    #               * A object of class 'difftime'
    #               * "sec", "min", "hour", "day", "DSTday", "week", "month", "year"
    from    <- zoo::as.Date(time_match(from))
    to      <- zoo::as.Date(time_match(to))
    temp    <- seq(from=from, to=to, by=by)
    if(trading){
        temp <- temp[RQuantLib::isBusinessDay(calendar=cal, dates=temp)]}
    return(temp)}

time_diff_days <- function(from, to, cal = "UnitedStates", trading=TRUE){
    from    <- zoo::as.Date(time_match(from))
    to      <- zoo::as.Date(time_match(to))
    temp    <- seq(from=from, to=to, by="day")
    if(trading){
        temp <- temp[RQuantLib::isBusinessDay(calendar=cal, dates=temp)]
        if(RQuantLib::isBusinessDay(calendar=cal, dates=from)){
            length(temp)-1
        }else{length(temp)}
    } else{length(temp)-1}}

############################# PLOT #######################################################
##########################################################################################
my3d_plot <- function(mat, xl="", yl="", zl="", FUN=persp3d){
    x       <- 1:nrow(mat)
    y       <- 1:ncol(mat)
    colorlut<- heat.colors(ncol(mat),alpha=0)
    col     <- rev(rep(colorlut,each=nrow(mat)))
    open3d(); bg3d("slategray"); material3d(col="black")
    FUN(x, y, mat, col = col, alpha=1, back="points", axes = TRUE, box = TRUE,front="lines",xlab=xl, ylab=yl, zlab=zl)}



