\name{weaver}
\alias{weaver}
\docType{data}
\title{
Oxidative stress and group size in social weaver birds
}
\description{
    Data from an experiment looking at the oxidative
    stress brought on by reproduction in white-browed 
    sparrow weavers Plocepasser mahali. The birds live
    in groups but only one pair reproduces. Either
    eggs were left alone or some eggs were removed
    to reduce the stress caused by reproduction. Two
    measures of oxidative stress were made.
}
\usage{data("weaver")}
\format{
  A data frame with 34 observations on the following 6 variables.
  \describe{
    \item{\code{Bird_ID}}{Individual bird identifier}
    \item{\code{Treatment}}{either control (eggs left) or some eggs removed to reduce the cost of reproduction}
    \item{\code{GrpSize}}{Size of the social group}
    \item{\code{SOD_final}}{Superoxide dismutase activity}
    \item{\code{MDA_final}}{Malondialdehyde concentration}
    \item{\code{mass_final}}{mass of the bird in g}
  }
}
\source{
Cram, Dominic L.; Blount, Jonathan D.; Young, Andrew J. (2015), Data from: The oxidative costs of reproduction are group-size dependent in a wild cooperative breeder, Dryad, Dataset, https://doi.org/10.5061/dryad.j1305
}
\references{
Cram, D.L., Blount, J.D. & Young, A.J. (2015) The oxidative costs of reproduction are group-size dependent in a wild cooperative breeder. Proceedings. Biological sciences / The Royal Society, 282, 20152031.
}
\examples{
data(weaver)
str(weaver)
}
\keyword{datasets}
