boxplot.CI <-
function(formula,data=NULL,group=NULL,layout=c(1,1),box=list(lwd=1,lty=2,col="royalblue1"),
    bars=list(lwd=1,lty=1,col="black",angle=90,pch=19,cex=1,CI=TRUE,alpha=0.05),
    stats=list(m=NULL,s=NULL,n=NULL,overlap=NULL), 
    join=FALSE,shift=0.2,ylim=NULL,main=NULL,...)
{
    if(!is.null(data)) {
        dataset <- get_all_vars(formula,data=data)
        group <- eval(substitute(group), envir = data)
        if(!is.null(group))
            dataset$group <- group
    } else {
        dataset <- get_all_vars(formula)
        if(!is.null(group))
            dataset$group <- group
    }
    dataset <- dataset[complete.cases(dataset),]
    # Gestione dei raggruppamenti
    if(!is.null(group)) {
        if(!is.factor(dataset$group))
            dataset$group <- as.factor(dataset$group)
        group.names <- levels(dataset$group)
        ng <- nlevels(dataset$group)
        if(layout[1]==1 & layout[2]==1)
            layout <- c(1,ng)
        if(is.null(main))
            main <- group.names
    } else {
        ng <- 1
    }
    # Aspetto del grafico
    if(is.null(box$lwd)) box$lwd <- 1
    if(is.null(box$lty)) box$lty <- 2
    if(is.null(box$col)) box$col <- "royalblue1"
    if(is.null(bars$lwd)) bars$lwd <- 1
    if(is.null(bars$lty)) bars$lty <- 1
    if(is.null(bars$col)) bars$col <- "black"
    if(is.null(bars$angle)) bars$angle <- 90
    if(is.null(bars$pch)) bars$pch <- 19
    if(is.null(bars$cex)) bars$cex <- 1
    if(is.null(bars$alpha)) bars$alpha <- 0.05
    if(is.null(ylim))
        ylim <- range(dataset[,1])
    double.stats <- !is.null(stats$overlap)
    if(!double.stats & is.null(stats$col))
        stats$col <- "red3"
    # Costruzione grafici
    par(mfrow=layout)
    shift.val <- shift
    # NB: statistiche personalizzate implementate solo se numero di gruppi = 1
    for(j in 1:ng) {
        if(ng==1) {
            y <- dataset[,1]
            x <- dataset[,2]
        } else {
            y <- dataset[dataset[,3]==group.names[j],1]
            x <- dataset[dataset[,3]==group.names[j],2]
        }
        if(!is.factor(x)) x <- as.factor(x)
        lev <- length(levels(x))
        shift <- 1:lev+shift.val
        m <- as.numeric(tapply(y,x,mean))
        s <- as.numeric(tapply(y,x,sd))
        n <- tapply(y,x, function(v) sum(!is.na(v)))
        SE <- s/sqrt(n)
        if(double.stats) {
            if(isTRUE(stats$overlap)) {
                m.new <- stats$m
                s.new <- stats$s
                n.new <- stats$n
                SE.new <- s.new/sqrt(n.new)
            } else {
                m <- stats$m
                s <- stats$s
                n <- stats$n
                SE <- s/sqrt(n)
                double.stats <- FALSE
            }
        }
        if(!is.null(bars$CI)) {
            if(bars$CI > 0) {
                SE <- qnorm(1-bars$alpha/2) * SE
                if(double.stats)
                    SE.new <- qnorm(1-bars$alpha/2) * SE.new
            }
            if(bars$CI < 0) {
                SE <- s
                if(double.stats)
                    SE.new <- s.new
            }
        }
        boxplot(y ~ x,
            boxcol=box$col,boxlwd=box$lwd,boxlty=box$lty,
            medcol=box$col,medlwd=box$lwd,medlty=box$lty,
            whiskcol=box$col,whisklwd=box$lwd,whisklty=box$lty,
            outcol=box$col,
            staplecol=box$col,staplelwd=box$lwd,staplelty=box$lty,
            main=main[j],ylim=ylim,...
        )
        if(!is.null(bars$CI))
            arrows(shift,m-SE,shift,m+SE,angle=bars$angle,code=3,lwd=bars$lwd,col=bars$col,length=0.1)
        points(shift,m,col=bars$col,pch=bars$pch,cex=bars$cex)
        if(!identical(FALSE, join)) {
            if(isTRUE(join))
                join <- 1:lev
            for(i in 1:length(join))
                segments(shift[join[i]],m[join[i]],shift[join[i+1]],m[join[i+1]],lwd=bars$lwd,col=bars$col)
        }
        if(double.stats) {
            if(!is.null(bars$CI))
                arrows(shift,m.new-SE.new,shift,m.new+SE.new,angle=bars$angle,code=3,lwd=bars$lwd,col=stats$col,length=0.1)
            points(shift,m.new,col=stats$col,pch=bars$pch,cex=bars$cex)
            if(!identical(FALSE, join)) {
                for(i in 1:length(join))
                    segments(shift[join[i]],m.new[join[i]],shift[join[i+1]],m.new[join[i+1]],lwd=bars$lwd,col=stats$col)
            }
        }
    }
    layout(1)
}
