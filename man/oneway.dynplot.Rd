\name{oneway.dynplot}
\alias{oneway.dynplot}
\title{Dynamite-plot univariato}
\description{
    Da descrivere
}
\usage{
oneway.dynplot(formula, data = NULL, ci = TRUE, alpha = 0.05, ...)
}
\arguments{
    \item{formula}{Da descrivere}
    \item{data}{Da descrivere}
    \item{ci}{Da descrivere}
    \item{alpha}{Da descrivere}
    \item{\dots}{Da descrivere}
}
\details{
    Da descrivere
}
\author{
Davide Massidda \email{davide.massidda@gmail.com}
}
\seealso{
\code{\link[explorer]{oneway.boxplot}}
}
\examples{
data(VPE)
oneway.dynplot(error ~ speed, data=VPE, ylim=c(-10,10))
}