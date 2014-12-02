\name{knn.impute}
\alias{knn.impute}
\title{Sostituzione dei dati mancanti con il metodo k-Nearest Neighbour}
\description{
Da descrivere
}
\usage{
knn.impute(x, k = NULL, distance = c("euclidean", "manhattan"),
           use = c("IC", "CC"), fun = weighted.mean)
}
\arguments{
\item{x}{Da descrivere}
\item{k}{Da descrivere}
\item{distance}{Da descrivere}
\item{use}{Da descrivere}
\item{fun}{Da descrivere}
}
\details{
Da descrivere
}
\value{
Da descrivere
}
\references{
Jönsson P. and C. Wohlin (2004). An Evaluation of k-nearest Neighbour Imputation
Using Likert Data. Proceedings 10th International Symposium pn Software Metrics,
pp. 108-118, Chiaco, USA. Selected for special issue.

Jönsson P. and C. Wohlin (2006). Benchmarking k-nearest neighbour imputation with
homogeneous Likert data. Empirical Software Engineering, 11, 463–489.
}
\author{
Davide Massidda <davide.massidda@gmail.com>
}
\examples{
data(airquiality)
colSums(is.na(airquality))
ariquality.imp <- knn.impute(airquality)
colSums(is.na(airquality.imp))
}
