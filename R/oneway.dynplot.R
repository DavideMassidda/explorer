oneway.dynplot <-
function(formula,data=NULL,ci=TRUE,alpha=0.05,...)
{
    if(!is.null(data))
        dataset <- model.frame(formula,data=data)
    else
        dataset <- model.frame(formula)
    y <- dataset[,1]
    x <- dataset[,2]
    m <- tapply(y, x, mean, na.rm=TRUE)
    s <- tapply(y, x, sd, na.rm=TRUE)
    n <- tapply(y, x, length)
    se <- s/sqrt(n)
    bp <- barplot(m,...)
    if(ci)
        z <- qnorm(1-alpha/2)
    else
        z <- 1
    arrows(x0 = bp, y0 = m, y1 = m+z*se, angle = 90)
    arrows(x0 = bp, y0 = m, y1 = m-z*se, angle = 90)
    # output <- list(mean=m,st.dev=s,n=n,st.error=se,z.value=z,ci=ci,alpha=alpha)
    # return(output)
}
