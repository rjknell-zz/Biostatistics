\name{gabon_diversity}
\alias{gabon_diversity}
\docType{data}
\title{
Data on relative animal abundances in Gabon
}
\description{
    Data on the relative abundance (proportion 
    of total observations) of a variety of 
    animal groups as recorded on a network of 
    transects in NE Gabon. A variety of land use
    variables are also recorded.
}
\usage{data("gabon_diversity")}
\format{
  A data frame with 24 observations on the following 14 variables.
  \describe{
    \item{\code{TransectID}}{Identifier for each transect}
    \item{\code{Distance}}{Distance from the nearest village in Km}
    \item{\code{HuntCat}}{Estimate of the intensity of hunting}
    \item{\code{LandUse}}{What the land is used for}
    \item{\code{NumHouseholds}}{Number of households in the nearest village}
    \item{\code{Veg_Rich}}{Vegetation species richness measured as the number of tree species present in a series of plots along the transect}
    \item{\code{Veg_Canopy}}{Canopy cover as the percentage of the sky blocked by canpoy in each plot, scored as 1 = 0-25, 2 = 26-50, 3 = 51-75, 4 = 76-100}
    \item{\code{Veg_Understory}}{Understory cover as the percentage of the ground covered, scored as for canopy cover}
    \item{\code{RA_Apes}}{Relative abundance of apes}
    \item{\code{RA_Birds}}{Relative abundance of birds}
    \item{\code{RA_Elephant}}{Relative abundance of elephants}
    \item{\code{RA_Monkeys}}{Relative abundance of monkeys}
    \item{\code{RA_Rodent}}{Relative abundance of rodents}
    \item{\code{RA_Ungulate}}{Relative abundance of ungulates}
  }
}
\source{
Koerner, Sally E. et al. (2017), Data from: Vertebrate community composition and diversity declines along a defaunation gradient radiating from rural villages in Gabon, Dryad, Dataset, https://doi.org/10.5061/dryad.vs97g
}
\references{
Koerner, S.E., Poulsen, J.R., Blanchard, E.J., Okouyi, J. & Clark, C.J. (2017) Vertebrate community composition and diversity declines along a defaunation gradient radiating from rural villages in Gabon (ed S Cheyne). The Journal of applied ecology, 54, 805-814.
}
\examples{
data(gabon_diversity)
## maybe str(gabon_diversity)
}
\keyword{datasets}
