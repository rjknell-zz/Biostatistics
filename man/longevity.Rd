\name{longevity}
\alias{longevity}
\docType{data}
\title{
Data on maximum lifespan for 909 species of mammal and bird
}
\description{
    A dataset used to explore the ecological correlates
    of longevity in mammals and birds.
}
\usage{data("longevity")}
\format{
  A data frame with 909 observations on the following 9 variables.
  \describe{
    \item{\code{species}}{Species name}
    \item{\code{class}}{Class: mammalia = mammals, aves = birds}
    \item{\code{order}}{Order}
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
