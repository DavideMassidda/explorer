na.replace <-
function(x, margin=2, fun=mean, ...)
{
    if(margin != 1 & margin != 2)
        stop("margin must be =1 or =2")
    if(margin == 2) {
        for(i in 1:ncol(x)) {
            na.check <- is.na(x[,i])
            if(sum(na.check) > 0)
                x[which(na.check),i] <- fun(x[-which(na.check),i],...)
        }
    } else {
        for(i in 1:nrow(x))
            na.check <- is.na(x[i,])
            if(sum(na.check) > 0)
                x[i,which(na.check)] <- fun(x[i,-which(na.check)],...)
    }
    return(x)
}
