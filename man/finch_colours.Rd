\name{finch_colours}
\alias{finch_colours}
\docType{data}
\title{
Finch colouration and mitochondrial function
}
\description{
    Data on the hue of the red feathers and a series of 
    measures of mitochondrial function in 36 male house 
    finches Haemorhous mexicanus at a time when they 
    were moulting and therefore actively producing red 
    carotenoids.
}
\usage{data("finch_colours")}
\format{
  A data frame with 36 observations on the following 5 variables.
  \describe{
    \item{\code{ID}}{ID number for the individual bird}
    \item{\code{Hue}}{Hue of the red feathers. Lower values are redder}
    \item{\code{C1RCR}}{"Respiratory Control Ratio", calculated by dividing the maximum respiration rate by the basal rate}
    \item{\code{C1MMP}}{Mitochondrial Membrane Potential}
    \item{\code{PGC_1a}}{The level of a protein which activates transcription in mitochondrial biogenesis}
  }
}
\source{
Hill, Geoffrey et al. (2019), Data from: Plumage redness signals mitochondrial function in the house finch, Dryad, Dataset, https://doi.org/10.5061/dryad.f0kr74v
}
\references{
Hill, G.E., Hood, W.R., Ge, Z., Grinter, R., Greening, C., Johnson, J.D., Park, N.R., Taylor, H.A., Andreasen, V.A., Powers, M.J., Justyn, N.M., Parry, H.A., Kavazis, A.N. & Zhang, Y. (2019) Plumage redness signals mitochondrial function in the house finch. Proceedings of the Royal Sociaty B: Biological sciences, 286, 20191354.
}
\examples{
data(finch_colours)
## maybe str(finch_colours)
}
\keyword{datasets}
