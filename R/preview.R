preview <- function(x, n = 6L, cols = 1:ncol(x))
{
    rows <- sample(seq_len(nrow(x)), size=n, replace=FALSE)
    rows <- sort(rows)
    return(x[rows,cols])
}
