\name{ragwort}
\alias{ragwort}
\docType{data}
\title{
Data on how plant-soil feedback affects growth of ragwort
}
\description{
    Data from an experiment looking at how the removal
    of soil biota affects plant-soil feedback and hence the
    growth of ragwort, Jacobaea vulgaris.
}
\usage{data("ragwort")}
\format{
  A data frame with 45 observations on the following 3 variables.
  \describe{
    \item{\code{inoculum}}{Mesh size of the filter used to process water from soil used to  inoculate otherwise sterile soil. Smaller mesh = less microbiota}
    \item{\code{root_mass}}{plant root mass in g (dry weight)}
    \item{\code{leaf_mass}}{plant leaf mass in g (dry weight)}
  }
}
\source{
Wang, M., Ruan, W., Kostenko, O., Carvalho, S., Hannula, S.E., Mulder, P.P.J., Bu, F., van der Putten, W.H. & Bezemer, T.M. (2019) Removal of soil biota alters soil feedback effects on plant growth and defense chemistry. The New Phytologist, 221, 1478-1491.
}
\examples{
data(ragwort)
str(ragwort)
}
\keyword{datasets}
