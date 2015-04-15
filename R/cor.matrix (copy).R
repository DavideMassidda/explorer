cor.matrix <-
function(x,y=NULL,z=NULL,na.action=c("listwise.deletion","pairwise.deletion","mean.replace"),method=c("pearson","kendall","spearman"),digits=3)
{
    # Verifica di na.action
    na.action <- tolower(na.action[1])
    na.action <- match.arg(na.action)
    # Verifica di method
    method <- tolower(method[1])
    method <- match.arg(method)
    if(is.vector(x)) {
        x.name <- deparse(substitute(x))
        if(grepl("\\$", x.name))
            x.name <- unlist(strsplit(x.name, "\\$"))[2]
        if(is.null(y))
            stop("You must provide both \'x\' and \'y\' or \'x\' in matrix form")
        else {
            y.name <- deparse(substitute(y))
            if(grepl("\\$", y.name))
                y.name <- unlist(strsplit(y.name, "\\$"))[2]
            x <- data.frame(x,y)
        }
        colnames(x) <- c(x.name,y.name)
    }
    cols <- ncol(x)
    n.missing <- matrix(0,nrow=cols,ncol=cols)
    if(na.action=="mean.replace") {
        m <- apply(x,2,mean,na.rm=TRUE)
        for(i in 1:cols) {
            na.check <- is.na(x[,i])
            if(sum(na.check)>0) {
                x[which(na.check),i] <- m[i]
            }
        }
        if(!is.null(z)) {
            na.check <- is.na(z)
            if(sum(na.check)>0)
                z[which(na.check)] <- mean(z,na.rm=TRUE)
        }
    } else {
        if(na.action=="listwise.deletion") {
            na.check <- rowSums(is.na(x))>0
            if(sum(na.check)>0) {
                x <- x[-which(na.check),]
                if(!is.null(z))
                    z <- z[-which(na.check)]
                n.missing[,] <- sum(na.check)
                diag(n.missing) <- NA
            }
        }
    }
    if(!is.null(z)) {
        na.check <- is.na(z)
        if(sum(na.check) > 0) {
            x <- x[-which(na.check),]
            z <- z[-which(na.check)]
            n.missing[,] <- n.missing[,]+sum(na.check)
            diag(n.missing) <- NA
        }
        for(i in 1:cols) {
            na.check <- is.na(x[,i])
            x.correct <- residuals(lm(x[,i] ~ z))
            if(sum(na.check) > 0)
                x[-which(na.check),i] <- x.correct
            else
                x[,i] <- x.correct
        }
    }
    rows <- nrow(x)
    x <- as.matrix(x)
    deg.fr <- matrix(rows-2,nrow=cols,ncol=cols)
    if(na.action == "pairwise.deletion" & sum(is.na(x)) > 0) {
        combin <- combn(cols,2)
        k <- ncol(combin)
        cor.values <- matrix(nrow=cols,ncol=cols)
        diag(cor.values) <- 1
        for(i in 1:k) {
            v1 <- x[,combin[1,i]]
            v2 <- x[,combin[2,i]]
            na.check <- is.na(v1) | is.na(v2)
            if(sum(na.check) > 0) {
                v1 <- v1[-which(na.check)]
                v2 <- v2[-which(na.check)]
            }
            n.missing[combin[2,i],combin[1,i]] <- n.missing[combin[2,i],combin[1,i]] + sum(na.check)
            n.missing[combin[1,i],combin[2,i]] <- n.missing[combin[1,i],combin[2,i]] + sum(na.check)
            deg.fr[combin[2,i],combin[1,i]] <- rows-sum(na.check)-2
            deg.fr[combin[1,i],combin[2,i]] <- rows-sum(na.check)-2
            cor.values[combin[2,i],combin[1,i]] <- cor(v1,v2,method=method)
            cor.values[combin[1,i],combin[2,i]] <- cor(v1,v2,method=method)
        }
    } else {
        cor.values <- cor(x,method=method)
    }
    t.values <- cor.values/sqrt((1-cor.values^2)/deg.fr)
    p.values <- 2*(1-pt(abs(t.values),deg.fr))
    diag(t.values) <- diag(p.values) <- diag(deg.fr) <- diag(n.missing) <- NA
    var.names <- colnames(x)
    colnames(t.values) <- colnames(deg.fr) <- colnames(p.values) <- colnames(n.missing) <- var.names
    rownames(t.values) <- rownames(deg.fr) <- rownames(p.values) <- rownames(n.missing) <- var.names
    output <- list(x=x,cor.values=cor.values,t.values=t.values,df=deg.fr,
        p.values=p.values,n.missing=n.missing,method=method,na.action=na.action,digits=digits)
    class(output) <- "cor.matrix"
    return(output)
}

print.cor.matrix <-
function(x,...)
{
    mat.dim <- ncol(x$cor.values)
    col.names <- colnames(x$x)
    row.names <- paste(formatC(col.names),"")
    col.names <- paste(formatC(col.names,width=-1),"")
    sig <- (x$p.values < 0.001) + (x$p.values < 0.01) +
        (x$p.values < 0.05) + (x$p.values < 0.1)
    sig <-
        ifelse(sig == 4, "***",
            ifelse(sig == 3, "** ",
                ifelse(sig == 2, "*  ",
                    ifelse(sig == 1,".  ","   ")
                )
            )
        )
    diag(sig) <- rep.int("   ",mat.dim)
    print.dig <- paste("% .",x$digits,"f",sep="")
    r <- sprintf(print.dig,x$cor.values)
    max.char <- max(nchar(c(col.names,r)))+1
    r <- formatC(paste(r,sig),width=-max.char)
    max.char <- max(nchar(r))
    col.names <- formatC(col.names,width=-max.char)
    col.names <- c(formatC("",width=max(nchar(row.names))),col.names)
    r <- matrix(r,nrow=mat.dim,ncol=mat.dim,byrow=TRUE)
    r <- cbind(row.names,r)
    for(i in 1:mat.dim) {
        if(i==1) cat(col.names,"\n")
        cat(r[i,],"\n")
    }
    cat("---\n")
    cat("Signif. codes:  0 \'***\' 0.001 \'**\' 0.01 \'*\' 0.05 \'.\' 0.1 \' \' 1\n")
}
