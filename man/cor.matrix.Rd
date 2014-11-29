\name{cor.matrix}
\alias{cor.matrix}
\title{Matrice di correlazione con significatività}
\description{Calcola la matrice di correlazione indicando le significatività.}
\usage{
cor.matrix(x, y = NULL, z = NULL, na.action = "listwise.deletion", method = "pearson")
}
\arguments{
    \item{x}{
        Un vettore numerico, una matrice o un data frame.
    }
    \item{y}{
        Vettore numerico da mettere un correlazione con \code{x}, quando
        \code{x} è anch'esso un vettore. La lunghezza di \code{y} deve
        essere compatibile con la lunghezza di \code{x}.
    }
    \item{z}{
        Variabile di controllo che si presume influenzi le altre variabili
        messe in correlazione. Se \code{z} viene specificato, gli indici in
        output saranno delle correlazioni parziali, calcolate ripulendo
        entrambe le variabili messe in relazione dall'influenza di \code{z}.
    }
    \item{na.action}{
        Modalità di trattamento dei dati mancanti (se presenti). Le possibili
        opzioni sono: "listwise.deletion", "pairwise.deletion" e "mean.replace"
        (vedi dettagli).        
    }
    \item{method}{
        Coefficiente di correlazione da calcolare. Le possibili opzioni sono:
        "pearson", "kendall" e "spearman".
    }
}
\details{
Se sono presenti dei dati mancanti (NA), l'argomento na.action può essere
usato per trattarli adeguatamente. Il metodo \code{listwise.deletion} elimina
l'intero caso quando viene individuato anche un solo NA nella riga; tutti gli
indici di correlazione verranno quindi calcolati solo a seguito dell'eliminazione
del caso.

Se viene applicato il metodo \code{pairwise.deletion}, un caso viene rimosso
soltanto quando questo presenta uno o più NA in una delle due variabili sotto
analisi. L'eliminazione non è quindi generalizzata, ma finalizzata al calcolo
dello specifico indice di correlazione. Attenzione: se è specificato un vettore
\code{z} che contiene NA, anche se il metodo specificato è \code{pairwise.deletion},
tutti i casi che presentano valori mancanti in \code{z} saranno rimossi.

Il metodo \code{mean.replace} sostituisce gli NA con la media aritmetica della
stessa variabile, calcolata omettendo i casi che presentano valori mancanti.

I metodi \code{listwise.deletion}, \code{pairwise.deletion} e \code{mean.replace}
possono essere più semplicemente richiamati come: \code{listwise}, \code{pairwise}
e \code{mean}.
}
\value{
    \item{x}{Matrice di dati.}
    \item{cor.values}{Indici di correlazione.}
    \item{t.values}{Valore della statistica t.}
    \item{df}{Gradi di libertà.}
    \item{p.values}{Valori di probabilità.}
    \item{method}{Coefficiente di correlazione utilizato.}
    \item{na.action}{Modalità di trattamento degli NA.}
}
\author{
Davide Massidda <davide.massidda@gmail.com>
}
\examples{
data(airquality)
cor.matrix(airquality, na.action="listwise.deletion")
cor.matrix(airquality, na.action="pairwise.deletion")
cor.matrix(airquality, na.action="mean.replace")
cor.matrix(airquality$Wind, airquality$Temp)
cor.matrix(airquality$Wind, airquality$Temp, airquality$Ozone)
}