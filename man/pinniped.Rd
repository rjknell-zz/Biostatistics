\name{pinniped}
\alias{pinniped}
\docType{data}
\title{
Pinniped brain sizes and mating system
}
\description{
    A data set of body and brain mass for 33
    species of pinniped (seals, sea lions and
    walruses) along with the type of mating
    system for that species.
}
\usage{data("pinniped")}
\format{
  A data frame with 33 observations on the following 6 variables.
  \describe{
    \item{\code{Species}}{Binomial species name}
    \item{\code{Male_brain_g}}{Brain mass for males in g}
    \item{\code{Female_brain_g}}{Brain mass for females in g}
    \item{\code{Male_mass_Kg}}{Body mass for males in Kg}
    \item{\code{Female_mass_Kg}}{Body mass for females in Kg}
    \item{\code{Mate_type}}{Mating system: mono = monogynous, poly = polygynous}
  }
}
\source{
https://onlinelibrary.wiley.com/doi/10.1111/j.1420-9101.2012.02520.x
}
\references{
Fitzpatrick, J.L., Almbro, M., Gonzalez-Voyer, A., Hamada, S., Pennington, C., Scanlan, J. & Kolm, N. (2012) Sexual selection uncouples the evolution of brain and body size in pinnipeds. Journal of evolutionary biology, 25, 1321-1330.
}
\examples{
data(pinniped)
str(pinniped)
}
\keyword{datasets}
