\name{cor.drop}
\alias{cor.drop}
\title{Identificazione dei casi influenti nella correlazione}
\description{
    Calcola la correlazione tra due variabili, omettendo, di volta in volta, un singolo caso.
    Se la rimozione di un caso è associata a una grossa variazione dell'indice di correlazione,
    questo caso può essere considerato influente.
}
\usage{
cor.drop(x, y = NULL, cases = NULL, delete.cases = NULL,
    na.action = "listwise.deletion", method = "pearson",
    alpha = 0.05)
}
\arguments{
    \item{x}{
        Variabile numerica da mettere in correlazione con \code{y}. Può essere
        sia un vettore che una matrice o un data frame. Se \code{x} è una matrice
        o un data frame, non sarà necessario specificare \code{y}; inoltre, se
        \code{x} presenta tre colonne, la terza colonna sarà utilizzata come codice
        identificativo dei casi (vedi argomento \code{cases}).
    }
    \item{y}{
        Variabile numerica da mettere in correlazione con \code{x}. Può essere
        omessa nel caso \code{x} sia una matrice o un data frame.
    }
    \item{cases}{
        Vettore che specifica il codice identificativo dei casi. Se omesso,
        a ogni caso verrà attribuito un codice numerico sequenziale. Può essere
        incluso in \code{x}, qualora questo sia una matrice o (meglio) un data frame.
    }
    \item{delete.cases}{
        Vettore di casi da non considerare.
    }
    \item{na.action}{
        Modalità di trattamento dei dati mancanti (se presenti). Le possibili
        opzioni sono: "listwise.deletion" e "mean.replace" (vedi dettagli).  
    }
    \item{method}{
        Coefficiente di correlazione da calcolare. Le possibili opzioni sono:
        "pearson", "kendall" e "spearman".
    }
    \item{alpha}{
        Valore alfa per il calcolo dell'intervallo di confidenza.
    }
}
\details{
Se sono presenti dei dati mancanti (NA), l'argomento na.action può essere
usato per trattarli adeguatamente. Il metodo \code{listwise.deletion} elimina
l'intero caso quando viene individuato anche un solo NA nella riga; tutti gli
indici di correlazione verranno quindi calcolati solo a seguito dell'eliminazione
del caso.

Il metodo \code{mean.replace} sostituisce gli NA con la media aritmetica della
stessa variabile, calcolata omettendo i casi che presentano valori mancanti.

Per la funzione \code{cor.drop}, il metodo pairwise non è implementato. Più
precisamente, questo può anche essere richiamato, ma darà i medesimi risultati
del metodo listwise, perché la funzione considera esclusivamente due variabili
per volta.

I metodi \code{listwise.deletion} e \code{mean.replace} possono essere più
semplicemente richiamati come: \code{listwise} e \code{mean}.
}
\value{
    \item{data}{Matrice di dati osservati.}
    \item{overall.test}{Test di significatività della correlazione su tutto il campione.}
    \item{method}{Coefficiente di correlazione utilizzato.}
    \item{correlations}{Correlazione calcolata omettendo i singoli casi.}
    \item{diff.ci}{Intervallo di confidenza degli scarti dalla correlazione globale}
    \item{na.action}{Modalità di trattamento degli NA.}

}
\author{
    Davide Massidda <davide.massidda@gmail.com>
}
\examples{
data(airquality)
aq <- airquality[1:20,]
r <- cor.drop(aq$Wind, aq$Temp)
r
plot(r)
}
