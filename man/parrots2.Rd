\name{parrots2}
\alias{parrots2}
\docType{data}
\title{
Parrot lifespan data
}
\description{
    A dataset on maximum lifespan for 69 species of parrot.
    A subset of the larger longevity dataset with further 
    information on family and subfamily added.
}
\usage{data("parrots2")}
\format{
  A data frame with 69 observations on the following 11 variables.
  \describe{
    \item{\code{species}}{Species name}
    \item{\code{class}}{Class: aves = birds}
    \item{\code{order}}{Order}
    \item{\code{family}}{Family}
    \item{\code{subfamily}}{Subfamily}
    \item{\code{maximum_lifespan_yr}}{Maximum lifespan in years}
    \item{\code{mass_g}}{Average body weight in g}
    \item{\code{volancy}}{Does it fly?}
    \item{\code{fossoriallity}}{Does it live in holes in the ground?}
    \item{\code{foraging_environment}}{Environment where it forages}
    \item{\code{daily_activity}}{When is it active?}
  }
}
\source{
https://royalsocietypublishing.org/doi/suppl/10.1098/rspb.2014.0298
}
\references{
Healy, K., Guillerme, T., Finlay, S., Kane, A., Kelly, S.B.A., McClean, D., Kelly, D.J., Donohue, I., Jackson, A.L. & Cooper, N. (2014) Ecology and mode-of-life explain lifespan variation in birds and mammals. Proceedings. Biological sciences / The Royal Society, 281, 20140298.
}
\examples{
data(longevity)
str(longevity)
}
\keyword{datasets}
