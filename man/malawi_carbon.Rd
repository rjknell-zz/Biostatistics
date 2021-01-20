\name{malawi_carbon}
\alias{malawi_carbon}
\docType{data}
\title{
Carbon exposure and lung immunity
}
\description{
    Data on two lung immunity measures (oxidative 
    burst activity and phagocytic activity) from
    people exposed to chronic carbon particulates
    in their homes in Malawi
}
\usage{data("malawi_carbon")}
\format{
  A data frame with 29 observations on the following 4 variables.
  \describe{
    \item{\code{X}}{Row number}
    \item{\code{log_carbon}}{Log carbon particulate exposure}
    \item{\code{oxidative_burst}}{Lung oxidative burst activity}
    \item{\code{phagocytosis}}{Phagocytic activity in the lung}
  }
}
\source{
Rylance, Jamie et al. (2016), Data from: Chronic household air pollution exposure is associated with impaired alveolar macrophage function in Malawian non-smokers, Dryad, Dataset, https://doi.org/10.5061/dryad.89nj3
}
\references{
Rylance, J., Chimpini, C., Semple, S., Russell, D.G., Jackson, M.J., Heyderman, R.S. & Gordon, S.B. (2015) Chronic Household Air Pollution Exposure Is Associated with Impaired Alveolar Macrophage Function in Malawian Non-Smokers. PloS one, 10, e0138762.
}
\examples{
data(malawi_carbon)
str(malawi_carbon)
}
\keyword{datasets}
