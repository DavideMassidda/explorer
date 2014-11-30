cor.drop <-
function(x, y=NULL, cases=NULL, delete.cases=NULL, na.action=c("listwise.deletion","mean.replace"),method=c("pearson","kendall","spearman"), alpha=0.05)
{
    # Verifica di na.action
    na.action <- tolower(na.action[1])
    na.action <- match.arg(na.action)
    # Verifica di method
    method <- tolower(method[1])
    mathod <- match.arg(method)
    # Riorganizzazione dati
    if(is.vector(x)) {
        if(is.null(y))
            stop("You must specify the vector \'y\'")
        else
            df <- data.frame(x,y)
        if(is.null(cases))
            df$cases <- 1:nrow(df)
        else
            df$cases <- cases
        df$cases <- as.character(df$cases)
     } else {
        df <- x
        if(is.matrix(df))
            df <- data.frame(df)
        if(ncol(df) == 2)
            df$cases <- as.character(1:nrow(df))
        colnames(df) <- c("x","y","cases")
    }
    if(na.action == "listwise.deletion" | na.action == "pairwise.deletion")
        df <- na.omit(df)
    else {
        na.check <- list(x = is.na(df$x), y = is.na(df$y))
        if(sum(na.check$x) > 0)
            df$x[which(na.check$x)] <- mean(df$x[-which(na.check$x)])
        if(sum(na.check$y) > 0)
            df$y[which(na.check$y)] <- mean(df$y[-which(na.check$y)])
    }
    na.check <- is.na(df$cases)
    if(sum(na.check) > 0)
        df$cases[which(na.check)] <- paste("NA",1:sum(na.check),sep=".")
    if(!is.null(delete.cases))
        df <- df[-which(df$cases %in% as.character(delete.cases)),]
    # Calcolo delle correlazioni
    r.test <- cor.test(df[,1],df[,2],method=method)
    n <- nrow(df)
    rdel <- numeric(n)
    for(i in 1:n)
        rdel[i] <- cor(df[-i,1],df[-i,2],method=method)
    r.diff <- rdel-r.test$estimate
    m <- mean(r.diff)
    z <- qnorm(1-alpha/2)
    se <- sd(r.diff)/sqrt(length(r.diff))
    output <- list(
            data = df,
            overall.test = r.test,
            method = method,
            correlations = data.frame(cases=df$cases,cor=rdel),
            diff.ci = c(lower=m-z*se,upper=m+z*se)+r.test$estimate,
            na.action = na.action
        )
    class(output) <- "cor.drop"
    return(output)
}

print.cor.drop <-
function(x,...)
{
    sig <- ifelse(x$overall.test$p.value < 0.001, "(p < 0.001)",
                  ifelse(x$overall.test$p.value < 0.01, "(p < 0.01)",
                         ifelse(x$overall.test$p.value < 0.05, "(p < 0.05)","(p = n.s.)")))
    method <- paste0(
        toupper(substr(x$method,1,1)),
        substr(x$method,2,nchar(x$method)),"\'s"
    )
    cat("\nOverall",method,"correlation: r =",round(x$overall.test$estimate,4),sig,"\n\n")
    cat("Correlations calculated deleting single cases:\n")
    x$correlations$cor <- round(x$correlations$cor,4)
    print(x$correlations)
}

plot.cor.drop <-
function(x, vertical=FALSE, main=NULL, axis.lim=NULL, mar.bottom=5.1, mar.left=4.1, mar.top=4.1, mar.right=2.1,...)
{
    y <- x$correlations$cor
    s <- x$correlations$cases
    r <- x$overall.test$estimate
    n <- nrow(x$data)
    # Definizione dettagli grafici:
    cor.lab <- paste0(
        toupper(substr(x$method,1,1)),
        substr(x$method,2,nchar(x$method)),
        "\'s correlation index"
    )
    mar <- c(mar.bottom,mar.left,mar.top,mar.right)
    cex <- 1.7
    vertical <- !vertical
    # Calcolo dei limiti dell'asse orizzontale
    lim <- range(y)
    fcround <- function(lim) {
        rnd <- round(lim,1)
        lim[1] <- ifelse(lim[1]-rnd[1] >=0, rnd[1], rnd[1]-0.1)
        lim[2] <- ifelse(lim[2]-rnd[2] <=0, rnd[2], rnd[2]+0.1)
        return(lim)
    }
    if(is.null(axis.lim))
        axis.lim <- lim <- fcround(lim=range(y))
    else
        lim <- fcround(lim=axis.lim)
    # Visualizzazione grafico:
    par(mar=mar)
    if(!vertical) {
        stripchart(y ~ s, vertical=vertical, las=2,
               pch=21, cex=cex, bg="grey",
               xlim=axis.lim, xlab="", ylab="")
        rect(x$diff.ci[1],0,x$diff.ci[2],n+1,col="gray90",border=NA)
        for(i in 1:n) segments(-1,i,1,i,col="gray70")
        for(i in seq(lim[1],lim[2],by=0.1))
            segments(i,0,i,n+1,lty=2,col=ifelse(round(i,1)!=0,"gray70","red3"))
        segments(r,0,r,n+1,lty=1,lwd=2,col="blue")
        par(mar=mar,new=TRUE)
        stripchart(y ~ s, vertical=vertical, las=2, pch=21, cex=cex, bg="grey",
            main=main, xlim=axis.lim, xlab=cor.lab, ylab="")
    } else {
        stripchart(y ~ s, vertical=vertical, las=2, pch=21, cex=cex, bg="grey",
               ylim=axis.lim, xlab="", ylab="")
        rect(0,x$diff.ci[1],n+1,x$diff.ci[2],col="gray90",border=NA)
        for(i in 1:n) segments(i,-1,i,1,col="gray70")
        for(i in seq(lim[1],lim[2],by=0.1))
            segments(0,i,n+1,i,lty=2,col=ifelse(round(i,1)!=0,"gray70","red3"))
        segments(0,r,n+1,r,lty=1,lwd=2,col="blue")
        par(mar=mar,new=TRUE)
        stripchart(y ~ s, vertical=vertical, las=2, pch=21, cex=cex, bg="grey",
            main=main, ylim=axis.lim, xlab="", ylab=cor.lab)
    }
}


