.round.odd <-
function(x)
{
    x <- c(floor(x),ceiling(x))
    x <- x[(x %% 2)==1]
    return(x[1])
}

knn.impute <-
function(x, k=NULL, distance=c("euclidean","manhattan"), use=c("IC","CC"), fun=weighted.mean)
{
    distance <- distance[1]
    distance <- match.arg(distance)
    use <- toupper(use[1])
    use <- match.arg(use)
    q <- switch(distance, euclidean = 2, manhattan = 1)
    isWMEAN <- deparse(substitute(fun)) == "weighted.mean"
    fun <- match.fun(fun)
    full.cases <- complete.cases(x)
    if(sum(full.cases) == 0)
        stop("Data must contain at least a complete case")
    if(is.null(k)) {
        k <- sqrt(sum(full.cases))
        k <- .round.odd(k)
    }
    full.cases <- which(full.cases)
    k <- seq_len(k)
    fun <- match.fun(fun)
    x.imputed <- x
    na.check <- list(miss = is.na(x), done = !is.na(x))
    na.pos <- which(rowSums(na.check$miss) > 0)
    for(i in 1:length(na.pos)) {
        miss.col <- which(na.check$miss[na.pos[i],])
        done.col <- which(na.check$done[na.pos[i],])
        for(j in 1:length(miss.col)) {
            browser()
            if(use == "IC") {
                filled.col <- c(miss.col[j],done.col)
                donors <- which(rowSums(is.na(x[,filled.col]))==0)
            } else
                donors <- full.cases
            recipients <- matrix(
                data = rep.int(as.numeric(x[na.pos[i],done.col]),length(donors)),
                nrow = length(donors), byrow = TRUE
            )
            distances <- rowSums(abs(recipients-x[donors,done.col])^q)
            neighbour <- x[donors[order(distances)],]
            if(isWMEAN)
                imputed.value <- weighted.mean(neighbour[k,miss.col[j]], w = distances[k])
            else
                imputed.value <- fun(neighbour[k,miss.col[j]])
            x.imputed[na.pos[i],miss.col[j]] <- imputed.value
        }
    }
    return(x.imputed)
}
