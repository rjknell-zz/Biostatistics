\name{gnatocerus}
\alias{gnatocerus}
\docType{data}
\title{
Weapon size and insulin-like signalling in the broad-horned flour beetle
}
\description{
    Data from an experiment testing how weapon size in
    insects is controlled by insulin-like peptides. The
    weapon in question is the enlarged mandibles of the
    broad-horned flour beetle Gnatocerus cornutus. 
    Larvae were treated with RNAi to knock out a number
    of candidate ILPs and adult size (elytron width) and
    weapon size were measured once they had eclosed.
}
\usage{data("gnatocerus")}
\format{
  A data frame with 144 observations on the following 3 variables.
  \describe{
    \item{\code{treatment}}{The RNAi knockout treatment used. GFP = control, 1-5 ILP are the 5 insulin-like peptides targetted}
    \item{\code{EW}}{Eytron width in micrometres}
    \item{\code{ML}}{Mandible length in micrometres}
  }
}
\source{
    https://figshare.com/s/609486022a3df39169bf
    DOI:10.6084/m9.figshare.9734780
}
\references{
Okada, Y., Katsuki, M., Okamoto, N., Fujioka, H. & Okada, K. (2019) A specific type of insulin-like peptide regulates the conditional growth of a beetle weapon. PLoS biology, 17, e3000541.
}
\examples{
data(gnatocerus)
str(gnatocerus)
}
\keyword{datasets}
