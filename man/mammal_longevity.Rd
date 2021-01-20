\name{mammal_longevity}
\alias{mammal_longevity}
\docType{data}
\title{
Data on maximum lifespan for 375 species of mammal
}
\description{
    A subset of a dataset used to explore the 
    ecological correlates of longevity in 
    mammals and birds, with only the mammals 
    represented.
}
\usage{data("longevity")}
\format{
  A data frame with 375 observations on the following 10 variables.
  \describe{
    \item{\code{X}}{Row number}
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
data(mammal_longevity)
str(mammal_longevity)
}
\keyword{datasets}
