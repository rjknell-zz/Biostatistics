\name{mhc}
\alias{mhc}
\docType{data}
\title{
MHC promiscuity and pathogen diversity data
}
\description{
    Data relating the breadth of the MHC response
    for the HLA-DRB1 MHC gene from a number of populations
    worldwide, and an estimate of the diversity of
    intracellular and extracellular pathogens that
    the population is exposed to

}
\usage{data("mhc")}
\format{
  A data frame with 28 observations on the following 4 variables.
  \describe{
    \item{\code{Population}}{Population identifier}
    \item{\code{Extracellular}}{Diversity of extracellular pathogens}
    \item{\code{Intracellular}}{Diversity of intracellular pathogens}
    \item{\code{Promiscuity_in_vitro}}{Estimated range of epitopes that the MHC can bind to}
  }
}
\source{
https://doi.org/10.1371/journal.pbio.3000131.s017
}
\references{
Manczinger, M., Boross, G., Kemeny, L., Muller, V., Lenz, T.L., Papp, B. & Pal, C. (2019) Pathogen diversity drives the evolution of generalist MHC-II alleles in human populations. PLoS biology, 17, e3000131.
}
\examples{
data(mhc)
str(mhc)
}
\keyword{datasets}
