outliers <- function(x, split = NULL, method = "boxplot", type = 7)
{
    x <- as.numeric(x)
    method <- method[1]
    method <- match.arg(method)
    if(is.null(split)) split <- 0
    lev <- levels(split)
    pos <- split(1:length(x), split)
    x <- split(x, split)
    if(method == "boxplot") {
        for(i in 1:length(x)) {
            q1 <- quantile(x[[i]],probs=0.25,na.rm=TRUE,type=type)
            q3 <- quantile(x[[i]],probs=0.75,na.rm=TRUE,type=type)
            iqr <- q3-q1
            bounds <- c(
                lower = max(c(min(x[[i]]),q1-1.5*iqr)),
                upper = min(c(max(x[[i]]),q3+1.5*iqr))
            )
            outpos <- which(x[[i]] < bounds[1] | x[[i]] > bounds[2])
            pos[[i]] <- pos[[i]][outpos]
        }
    }
    pos <- as.numeric(unlist(pos))
    return(pos)
}
