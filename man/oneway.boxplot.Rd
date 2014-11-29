\name{oneway.boxplot}
\alias{oneway.boxplot}
\title{Box-plot univariato}
\description{
    Per ogni livello del fattore, sovrappone la media e il suo l'errore standard al rispettivo
    boxplot.
}
\usage{
oneway.boxplot(formula, data, box = list(lwd = 1, lty = 2, col = "royalblue1"),
    bars = list(lwd = 1, lty = 1, col = "black", angle = 90,
    pch = 19, cex = 1, join = NULL), shift = 0.2, ...)
}
\arguments{
    \item{x}{Variabile indipendente di tipo fattore.}
    \item{y}{Variabile dipendente.}
    \item{box}{Specifiche grafiche da passare alla funzione \code{boxplot}.}
    \item{bars}{Specifiche grafiche delle linee.}
    \item{shift}{Scostamento dal centro del boxplot delle barre delle media.}
    \item{...}{Ulteriori argomenti da passare alla funzione \code{boxplot}.}
}
\details{
Da descrivere.
}
\author{
Davide Massidda \email{davide.massidda@gmail.com}
}
\seealso{
\code{\link[explorer]{oneway.dynplot}}
}
\examples{
data(VPE)
oneway.boxplot(error ~ speed, data=VPE)
}