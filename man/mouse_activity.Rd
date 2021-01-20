\name{mouse_activity}
\alias{mouse_activity}
\docType{data}
\title{
Locomotor activity in offspring of mice exposed to nicotine
}
\description{
    Data on the spontaneous locomotor activity
    of the F1 offspring of nicotine exposed or 
    control males mated with unexposed females
    over a 12 hour period.
}
\usage{data("mouse_activity")}
\format{
  A data frame with 54 observations on the following 3 variables.
  \describe{
    \item{\code{Treatment}}{Parental treatment}
    \item{\code{Sex}}{Sex of the animal in question}
    \item{\code{SLA}}{Spontaneous locomotor activity (the count of all the recorded times an infra-red beam was broken during the 12-hour period)}
  }
}
\source{
https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.2006497
}
\references{
McCarthy, D.M., Morgan, T.J., Jr, Lowe, S.E., Williamson, M.J., Spencer, T.J., Biederman, J. & Bhide, P.G. (2018) Nicotine exposure of male mice produces behavioral impairment in multiple generations of descendants. PLoS biology, 16, e2006497.
}
\examples{
data(mouse_activity)
str(mouse_activity)
}
\keyword{datasets}
