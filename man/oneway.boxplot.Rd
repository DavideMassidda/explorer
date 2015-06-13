\name{oneway.boxplot}
\alias{oneway.boxplot}
\title{Box-plot a una via}
\description{
    Per ogni livello del fattore, sovrappone la media e il suo intervallo di confidenza al relativo boxplot.
}
\usage{
oneway.boxplot(formula, data, box = list(lwd = 1, lty = 2, col = "royalblue1"),
    bars = list(lwd = 1, lty = 1, col = "black", angle = 90, pch = 19, cex = 1,
    CI = TRUE, alpha = 0.05), join = FALSE, shift = 0.2, ...)
}
\arguments{
    \item{formula}{Formula che indica la variabile dipendente e la variabile indipendente (di tipo fattore).}
    \item{data}{Data frame che contiene le variabili.}
    \item{box}{Specifiche grafiche da passare alla funzione \code{boxplot}.}
    \item{bars}{Specifiche grafiche dei segmenti che descrivono le barre d'errore intorno alle medie.}
    \item{join}{Valore logico. Se \code{TRUE}, le medie saranno congiunte attraverso dei segmenti.}
    \item{shift}{Scostamento dal centro del boxplot dei marker che identificano le medie.}
    \item{...}{Ulteriori argomenti da passare alla funzione \code{boxplot}.}
}
\details{
La lista \code{bars} può contenere i seguenti elementi:
lwd: spessore del segmento.
lty: tipo di segmento.
col: colore del segmento.
angle: angolazione della parte terminale del segmento.
CI: valore logico; se \code{TRUE}, le barre d'errore rappresenteranno l'intervallo di confidenza,
    se \code{FALSE}, le barre rappresenteranno l'errore standard.
alpha: valore critico per il calcolo dell'intervallo di confiedenza.
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
