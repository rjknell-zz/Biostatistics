\name{carnivores}
\alias{carnivores}
\docType{data}
\title{
Brain and body mass for carnivore species
}
\description{
Data on mean brain and body mass for 199 species from the carnivora.
}
\usage{data("carnivores")}
\format{
  A data frame with 199 observations on the following 7 variables.
  \describe{
    \item{\code{Binomial}}{binomial species name}
    \item{\code{order}}{order name}
    \item{\code{family}}{family name}
    \item{\code{genus}}{genus name}
    \item{\code{species}}{species name}
    \item{\code{Mean_brain_mass_g}}{mean brain mass for that species in g}
    \item{\code{Mean_body_mass_Kg}}{mean body mass for that species in Kg}
  }
}
\source{
Burger, Joseph Robert; George, Menshian Ashaki; Leadbetter, Claire; Shaikh, Farhin (2019), Data from: The allometry of brain size in mammals, Dryad, Dataset, https://doi.org/10.5061/dryad.2r62k7s
}
\references{
Burger, J.R., George, M.A., Leadbetter, C. & Shaikh, F. (2019) The allometry of brain size in mammals. Journal of mammalogy, 100, 276-283.
}
\examples{
data(carnivores)
str(carnivores)
}
\keyword{datasets}
