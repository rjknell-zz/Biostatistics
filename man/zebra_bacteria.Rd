\name{zebra_bacteria}
\alias{zebra_bacteria}
\docType{data}
\title{
Data on bacterial adaptation to host gut environments.
}
\description{
    Data from a study on bacterial adaptation
    to host gut environments. These data are
    measures of  the competitive ability a 
    bacterium called Aeromonas veronii after
    either 4 or 18 passages through the gut of
    an otherwise germ-free larval zebrafish.
}
\usage{data("zebra_bacteria")}
\format{
  A data frame with 208 observations on the following 4 variables.
  \describe{
    \item{\code{Line}}{Which of three lines}
    \item{\code{Host}}{Wild type (WT) or an immunodeficient myd88- mutant}
    \item{\code{Passage}}{Number of passages}
    \item{\code{CI}}{Competitive index}
  }
}
\source{
https://figshare.com/articles/dataset/Experimental_bacterial_adaptation_to_the_zebrafish_gut_reveals_a_primary_role_for_immigration/7445246
}
\references{
Robinson, C.D., Klein, H.S., Murphy, K.D., Parthasarathy, R., Guillemin, K. & Bohannan, B.J.M. (2018) Experimental bacterial adaptation to the zebrafish gut reveals a primary role for immigration. PLoS biology, 16, e2006893.
}
\examples{
data(zebra_bacteria)
str(zebra_bacteria)
}
\keyword{datasets}
