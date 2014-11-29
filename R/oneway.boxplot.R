oneway.boxplot <-
function(formula,data,box=list(lwd=1,lty=2,col="royalblue1"),
    bars=list(lwd=1,lty=1,col="black",angle=90,pch=19,cex=1,join=NULL),shift=0.2,...)
{
    if(!is.null(data))
        dataset <- model.frame(formula,data=data)
    else
        dataset <- model.frame(formula)
    y <- dataset[,1]
    x <- dataset[,2]
    na.check <- is.na(y) | is.na(x)
    if(sum(na.check)>0) {
        y <- y[!na.check]
        x <- x[!na.check]
    }
    if(!is.factor(x)) x <- as.factor(x)
    if(is.null(box$lwd)) box$lwd <- 1
    if(is.null(box$lty)) box$lty <- 2
    if(is.null(box$col)) box$col <- "royalblue1"
    if(is.null(bars$lwd)) bars$lwd <- 1
    if(is.null(bars$lty)) bars$lty <- 1
    if(is.null(bars$col)) bars$col <- "black"
    if(is.null(bars$angle)) bars$angle <- 90
    if(is.null(bars$pch)) bars$pch <- 19
    if(is.null(bars$cex)) bars$cex <- 1
    lev <- length(levels(x))
    shift <- 1:lev+shift
    m <- as.numeric(tapply(y,x,mean))
    s <- as.numeric(tapply(y,x,sd))
    n <- tapply(y,x,length)
    SE <- s/sqrt(n)
    boxplot(y ~ x,
        boxcol=box$col,boxlwd=box$lwd,boxlty=box$lty,
        medcol=box$col,medlwd=box$lwd,medlty=box$lty,
        whiskcol=box$col,whisklwd=box$lwd,whisklty=box$lty,
        outcol=box$col,
        staplecol=box$col,staplelwd=box$lwd,staplelty=box$lty,...
    )
    arrows(shift,m-SE,shift,m+SE,angle=bars$angle,code=3,lwd=bars$lwd,col=bars$col)
    points(shift,m,col=bars$col,pch=bars$pch,cex=bars$cex)
    if(is.null(bars$join)) bars$join <- 1:lev
    for(i in bars$join)
        segments(shift[i],m[i],shift[i+1],m[i+1],lwd=bars$lwd,col=bars$col)
}
