\name{na.replace}
\alias{na.replace}
\title{Anteprima di un dataset}
\description{
  Da descrivere
}
\usage{
preview(x, n = 6L, cols = 1:ncol(x))
}
\arguments{
\item{x}{Oggetto di tipo \code{data.frame} o \code{matrix}.}
\item{n}{Numero di righe da visualizzare.}
\item{cols}{Indici o nomi di colonna.}
}
\details{
Da descrivere
}
\value{
Da descrivere
}
\references{
Da descrivere
}
\author{
Davide Massidda <davide.massidda@gmail.com>
}
\seealso{
    \code{\link[utils]{head}}, \code{\link[utils]{tail}}
}
\examples{
data(airquality)
preview(airquality)
}
