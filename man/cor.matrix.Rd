\name{cor.matrix}
\alias{cor.matrix}
\title{Matrice di correlazione con significatività}
\description{Calcola la matrice di correlazione indicando le significatività.}
\usage{
cor.matrix(x, y = NULL, z = NULL,
           na.action = c("listwise.deletion", "pairwise.deletion", "mean.replace"),
           half.view=c("lower","upper","none"),
           method = c("pearson", "kendall", "spearman"), digits = 3)
}
\arguments{
    \item{x}{
        Vettore numerico, matrice o data frame.
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
    \item{half.view}{
        Indica quale porzione della matrice di correlazione visualizzare in
        output. Possibili opzioni sono: "lower", "upper" e "none".
    }
    \item{digits}{
        Numero di cifre decimali da visualizzare in output.
    }
}
\details{
Se sono presenti dei dati mancanti (NA), l'argomento na.action può essere
usato per trattarli adeguatamente. Il metodo \code{listwise.deletion} elimina
l'intero caso quando viene individuato anche un solo NA nella riga; tutti gli
indici di correlazione verranno quindi calcolati solo a seguito dell'eliminazione
del caso.

Se viene applicato il metodo \code{pairwise.deletion}, un caso viene rimosso
soltanto quando presenta uno o più NA in una delle due variabili sotto analisi
e la sua eliminazione è contestuale al calcolo del singolo indice.
L'eliminazione non è quindi generalizzata, ma finalizzata al calcolo dello specifico
indice di correlazione. Attenzione: se è specificato un vettore \code{z} che
contiene NA, anche se il metodo specificato è \code{pairwise.deletion}, tutti i
casi che presentano valori mancanti in \code{z} saranno rimossi.

Il metodo \code{mean.replace} sostituisce gli NA con la media aritmetica della
stessa variabile, calcolata omettendo i casi che presentano valori mancanti.
}
\value{
    \item{x}{Matrice di dati.}
    \item{cor.values}{Indici di correlazione.}
    \item{t.values}{Valore della statistica t.}
    \item{df}{Gradi di libertà.}
    \item{p.values}{Valori di probabilità.}
    \item{method}{Coefficiente di correlazione utilizato.}
    \item{na.action}{Modalità di trattamento degli NA.}
    \item{half.view}{Porzione della matrice da visualizzate.}
    \item{digits}{Numero di cifre decimali degli indici.}
}
\author{
Davide Massidda <davide.massidda@gmail.com>
}
\examples{
data(airquality)
cor.matrix(airquality, na.action="listwise")
cor.matrix(airquality, na.action="pairwise")
cor.matrix(airquality, na.action="mean")
cor.matrix(airquality$Wind, airquality$Temp)
cor.matrix(airquality$Wind, airquality$Temp, airquality$Ozone)
}
