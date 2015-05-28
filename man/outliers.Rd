\name{outliers}
\alias{outliers}
\title{Individuazione dei valori anomali}
\description{
    All'interno di un vettore, la funzione individua le posizioni dei valori anomali secondo un certo
    criterio prestabilito. Attualmente, l'unico criterio disponibile è quello del boxplot.
}
\usage{
outliers(x, split = NULL, method = "boxplot", type = 7)
}
\arguments{
    \item{x}{Vettore di dati numerici.}
    \item{split}{Fattore di raggruppamento dei dati in \code{x}. Se \code{split} viene specificato, i dati
      in \code{x} saranno suddivisi a seconda del livello del fattore di raggruppamento a cui appartengono
      e gli outlier saranno ricercati all'interno dei singoli raggruppamenti.}
    \item{method}{Metodo per l'identificazione degli outlier. Attualmente, l'unico metodo implementato è
      \code{boxplot}, che identifica come outlier i valori che si posizionano oltre i limiti definiti dai
      "baffi".}
    \item{type}{Un numero intero da 1 a 9 per selezionare uno dei nove algoritmi per il calcolo dei quantili
      per il metodo \code{boxplot} (si veda la funzione \code{quantile} per i dettagli.)}
}
\details{
  La funzione restituisce le posizioni in \code{x} in cui si trovano i valori anomali. Queste posizioni
  possono essere utilizzare per isolare i valori anomali.
}
\value{
  Un vettore di interi che corrispondono alle posizioni degli outlier nel vettore \code{x}.
}
\references{
    Da descrivere
}
\author{
Davide Massidda <davide.massidda@gmail.com>
}
\examples{
data(ChickWeight)
boxplot(weight ~ Diet, data=ChickWeight)
out <- with(ChickWeight, outliers(weight,Diet))
ChickWeight[out, ]
}
