cor.matrix <-
function(x,y=NULL,z=NULL,na.action="listwise.deletion",method="pearson")
{
    # Verifica di na.action
    na.action <- tolower(na.action[1])
    if(na.action == "mean") na.action <- "mean.replace"
    if(na.action == "listwise") na.action <- "listwise.deletion"
    if(na.action == "pairwise") na.action <- "pairwise.deletion"
    na.action.check <- na.action == c("mean.replace", "listwise.deletion", "pairwise.deletion")
    if(sum(na.action.check) == 0)
        stop("na.action not identified")
    # Verifica di method
    method <- tolower(method[1])
    method.check <- method == c("pearson", "kendall", "spearman")
    if(sum(method.check) == 0)
        stop("method not identified")
    if(is.vector(x)) {
        x.name <- deparse(substitute(x))
        if(grepl("\\$", x.name))
            x.name <- unlist(strsplit(x.name, "\\$"))[2]
        if(is.null(y))
            error("You must provide both \'x\' and \'y\' or \'x\' in matrix form")
        else {
            y.name <- deparse(substitute(y))
            if(grepl("\\$", y.name))
                y.name <- unlist(strsplit(y.name, "\\$"))[2]
            x <- data.frame(x,y)
        }
        colnames(x) <- c(x.name,y.name)
    }
    cols <- ncol(x)
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
            }
        }
    }
    if(!is.null(z)) {
        na.check <- is.na(z)
        if(sum(na.check) > 0) {
            x <- x[-which(na.check),]
            z <- z[-which(na.check)]
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
            deg.fr[combin[2,i],combin[1,i]] <- deg.fr[combin[1,i],combin[2,i]] <- rows-sum(na.check)-2
            cor.values[combin[2,i],combin[1,i]] <- cor.values[combin[1,i],combin[2,i]] <- cor(v1,v2,method=method)
        }
    } else {
        cor.values <- cor(x,method=method)
    }
    t.values <- cor.values/sqrt((1-cor.values^2)/deg.fr)
    p.values <- 2*(1-pt(abs(t.values),deg.fr))
    diag(t.values) <- diag(p.values) <- NA
    var.names <- colnames(x)
    colnames(t.values) <- colnames(deg.fr) <- colnames(p.values) <- var.names
    rownames(t.values) <- rownames(deg.fr) <- rownames(p.values) <- var.names
    output <- list(x=x,cor.values=cor.values,t.values=t.values,
        df=deg.fr,p.values=p.values,method=method,na.action=na.action)
    class(output) <- "cor.matrix"
    return(output)
}

print.cor.matrix <-
function(object)
{
    mat.dim <- ncol(object$cor.values)
    col.names <- row.names <- colnames(object$x)
    sig <- (object$p.values < 0.001) + (object$p.values < 0.01) +
        (object$p.values < 0.05) + (object$p.values < 0.1)
    sig <-
        ifelse(sig == 4, "*** ",
            ifelse(sig == 3, "**  ",
                ifelse(sig == 2, "*   ",
                    ifelse(sig == 1,".   ","    ")
                )
            )
        )
    # Valori da riportare sulla diagonale:
    diag(sig) <- rep.int("    ",mat.dim)
    empty <- paste(rep(" ",50),collapse=" ") # emorme stringa vuota
    maxChar <- max(nchar(col.names)) # massimo numero di caratteri in una stringa
    stringAdd <- " " # spazio tra nomi di riga e valori nella prima colonna
    # 7 e' il munero di caratteri delle cifre in colonna
    # A questi deve essere aggiunto nchar(stringAdd), che e' il numero di caratteri
    # che fanno da spaziatore tra i nomi di riga e i valori nella prima colonna.
    # Il numero di caratteri della prima riga deve essere almeno 7:
    if(nchar(col.names[1]) < (7 + nchar(stringAdd))) {
        if(nchar(col.names[1]) < 7)
            col.names[1] <- paste(substr(empty,1,7-nchar(col.names[1])),col.names[1],sep="")
        col.names[1] <- paste(stringAdd,col.names[1],sep="")
    }
    if(nchar(col.names[1]) > (7 + nchar(stringAdd)))
        stringAdd <- substr(empty,1,nchar(col.names[1])-7)
    # Aggiunta del numero di caratteri dei nomi di riga:
    charNames <- nchar(row.names)
    maxChar <- max(charNames)
    col.names[1] <- paste(substr(empty,1,maxChar+2),col.names[1],sep="")
    sig <- cbind(stringAdd,sig)
    # Aggiunta di caratteri a ogni stringa per uguagliarle almeno alla lunghezza dei numeri:
    for(i in 2:mat.dim) {
        if(charNames[i]<11) {
            ladd <- 11-charNames[i]+1
            col.names[i]<- paste(substr(empty,1,ladd),col.names[i],sep="")
        }
        if(charNames[i]>11) {
            col.names[i]<- paste(substr(empty,1,1),col.names[i],sep="")
            sig[,i] <- paste(sig[,i],substr(empty,1,charNames[i]-11),sep="")
        }
    }
    charNames <- nchar(row.names)
    # posizioni occupate dalle stringhe piu' lunghe:
    posMaxChar <- which(charNames==maxChar)
    # posizioni occupate dalle stringhe piu' corte:
    posMinChar <- 1:mat.dim
    if(length(posMaxChar)!=0)
        posMinChar <- posMinChar[-posMaxChar]
    # Omologazione dei nomi di riga:
    for(i in posMinChar) {
        # numero di caratteri vuoti da aggiungere:
        ladd <- maxChar-charNames[i]
        # Amplia le stringhe piu' corte:
        row.names[i]<- paste(substr(empty,1,ladd),row.names[i],sep="")
    }
    sig_empty <- sig[,1]
    sig <- sig[,-1]
    diagChar <- substr(empty,1,7)
    for(j in 1:mat.dim) cat(col.names[j])
    k <- 0
    for(i in 1:mat.dim) {
        k <- k+1
        cat("\n",row.names[i],stringAdd)
        for(j in 1:k) {
            #if(i!=j)
                cat(sprintf("% .4f",object$cor.values[i,j]),sig[i,j])
            #else
            #    cat(sprintf("%s",diagChar),sig[i,j])
        }
    }
    cat("\n---\n")
    cat("Signif. codes:  0 \'***\' 0.001 \'**\' 0.01 \'*\' 0.05 \'.\' 0.1 \' \' 1\n")
}
