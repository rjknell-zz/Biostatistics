\name{latitude_diversity}
\alias{latitude_diversity}
\docType{data}
\title{
Tree diversity data
}
\description{
    Data on tree diversity from 24 forest plots 
    in locations ranging from the tropics to 
    northern Europe and the USA
}
\usage{data("latitude_diversity")}
\format{
  A data frame with 24 observations on the following 12 variables.
  \describe{
    \item{\code{Plot}}{Plot name}
    \item{\code{Country}}{Country}
    \item{\code{Latitude}}{Plot latitude}
    \item{\code{Longitude}}{Plot longitude}
    \item{\code{Plot_size_Ha}}{Size of the plot in Ha}
    \item{\code{Total_individuals}}{Total number of trees >1cm DBH}
    \item{\code{Species_richness}}{Total number of tree species}
    \item{\code{Rarified_sp_richness}}{Rarified species richness}
    \item{\code{Shannon_diversity}}{Shannon diversity index}
    \item{\code{Mean_local_richness}}{Mean richness per 20m quadrat}
    \item{\code{Mean_local_richness_rarified}}{Rarified richness per 20m quadrat}
    \item{\code{Mean_local_Shannon}}{Shannon diversity index per 20m quadrat}
  }
}
\source{
Table S1 in https://science.sciencemag.org/content/sci/suppl/2017/06/28/356.6345.1389.DC1/aam5678_LaManna_SM.pdf
}
\references{
LaManna, J.A., Mangan, S.A., Alonso, A., Bourg, N.A., Brockelman, W.Y., Bunyavejchewin, S., Chang, L.-W., Chiang, J.-M., Chuyong, G.B., Clay, K., Condit, R., Cordell, S., Davies, S.J., Furniss, T.J., Giardina, C.P., Gunatilleke, I.A.U.N., Gunatilleke, C.V.S., He, F., Howe, R.W., Hubbell, S.P., Hsieh, C.-F., Inman-Narahari, F.M., Janik, D., Johnson, D.J., Kenfack, D., Korte, L., Kral, K., Larson, A.J., Lutz, J.A., McMahon, S.M., McShea, W.J., Memiaghe, H.R., Nathalang, A., Novotny, V., Ong, P.S., Orwig, D.A., Ostertag, R., Parker, G.G., Phillips, R.P., Sack, L., Sun, I.-F., Tello, J.S., Thomas, D.W., Turner, B.L., Vela Diaz, D.M., Vrska, T., Weiblen, G.D., Wolf, A., Yap, S. & Myers, J.A. (2017) Plant diversity increases with the strength of negative density dependence at the global scale. Science, 356, 1389-1392.
}
\examples{
data(latitude_diversity)
str(latitude_diversity)
}
\keyword{datasets}
