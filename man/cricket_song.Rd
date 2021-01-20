\name{cricket_song}
\alias{cricket_song}
\docType{data}
\title{
Cricket song dataset
}
\description{
    Data on condition, resource availability (food quality)
    and sexual signalling (calling song) in the decorated 
    cricket, Gryllodes sigillatus.
}
\usage{data("cricket_song")}
\format{
  A data frame with 568 observations on the following 5 variables.
  \describe{
    \item{\code{Diet}}{total nutritional content of the diet in percent}
    \item{\code{Pronotum}}{Pronotum width in mm}
    \item{\code{Mass0}}{mass at the start of the experiment in g}
    \item{\code{Delta_smi}}{change in weight over the first week in g}
    \item{\code{Song_week1}}{Total amount of time the animal sang in the first week of the experiment in seconds}
  }
}
\source{
Houslay, Thomas M. et al. (2017), Data from: Mating opportunities and energetic constraints drive variation in age-dependent sexual signalling, Dryad, Dataset, https://doi.org/10.5061/dryad.tj693
}
\references{
Houslay, T.M., Houslay, K.F., Rapkin, J., Hunt, J. & Bussi??re, L.F. (2017) Mating opportunities and energetic constraints drive variation in age-dependent sexual signalling (ed C Miller). Functional ecology, 31, 728-741.
}
\examples{
data(cricket_song)
str(cricket_song)
}
\keyword{datasets}
