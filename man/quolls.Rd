\name{quolls}
\alias{quolls}
\docType{data}
\title{
Data on physical performance in Northern Quolls
}
\description{
    Data from a study of physical performance and
    morphology in Northern Quolls, Dasyurus  hallucatus.
    These data were collected to test hypotheses about
    trade-offs between different types of physical
    activity. NB all physical measurements are standardised
    to a mean of 0 and an SD of 1.
}
\usage{data("quolls")}
\format{
  A data frame with 63 observations on the following 23 variables.
  \describe{
    \item{\code{name}}{Number of the individual}
    \item{\code{sex}}{Sex}
    \item{\code{mass}}{mass in g}
    \item{\code{bodylength}}{length in mm}
    \item{\code{taillength}}{tail length in mm}
    \item{\code{taildiameter}}{tail diamter in mm}
    \item{\code{headwidth}}{head width in mm}
    \item{\code{headlength}}{head length in mm}
    \item{\code{meanforearm}}{mean forearm length in mm}
    \item{\code{meanhindlimb}}{mean hind limb length in mm}
    \item{\code{meanfootlength}}{mean length of feet in mm}
    \item{\code{bodysize_PC1}}{PC1 of body size measurements}
    \item{\code{acceleration}}{Acceleration}
    \item{\code{sprint}}{sprint speed}
    \item{\code{jump}}{jump speed}
    \item{\code{grasp}}{grasping strength}
    \item{\code{bite}}{bite force}
    \item{\code{motorcontrol}}{A measure of motor control}
    \item{\code{maneuverability}}{manauverability}
    \item{\code{Max_O2_consump}}{Maximum oxygen consumption}
    \item{\code{Performance_PC1}}{Principal component 1 from a PCA of performance variables}
    \item{\code{Performance_PC2}}{Principal component 2 from a PCA of performance variables}
    \item{\code{Performance_PC3}}{Principal component 3 from a PCA of performance variables}
  }
}
\source{
Charters, Jordan E. et al. (2019), Data from: Multidimensional analyses of physical performance reveal a size dependent trade-off between suites of traits, Dryad, Dataset, https://doi.org/10.5061/dryad.k0v636g
}
\references{
Charters, J.E., Heiniger, J., Clemente, C.J., Cameron, S.F., Amir Abdul Nasir, A.F., Niehaus, A.C. & Wilson, R.S. (2018) Multidimensional analyses of physical performance reveal a size-dependent trade-off between suites of traits. Functional ecology, 32, 1541-1553.
}
\examples{
data(quolls)
str(quolls)
}
\keyword{datasets}
