\name{height_immunity}
\alias{height_immunity}
\docType{data}
\title{
Data on the relationship between immune system functioning and body height in healthy people. 
}
\description{
    Data from a study relating innate (complement & lysozyme activity,
    neutrophil function) and adaptive (lymphocyte count, IgA, IgG and
    vaccine responce) to height. The subjects were a group of Polish
    volunteers.
}
\usage{data("height_immunity")}
\format{
  A data frame with 193 observations on the following 16 variables.
  \describe{
    \item{\code{sex}}{Subject sex}
    \item{\code{age}}{Age in years}
    \item{\code{body.height}}{Height in mm}
    \item{\code{BMI}}{Body Mass Index}
    \item{\code{body_fat}}{Percent body fat}
    \item{\code{testosterone}}{Free testosterone titre}
    \item{\code{complement}}{Complement activity}
    \item{\code{lysozyme}}{Lysozyme activity}
    \item{\code{phagocytic}}{Phagocytic uptake}
    \item{\code{ROS}}{Reactive Oxygen Species production}
    \item{\code{IgA}}{Immunoglobulin A levels}
    \item{\code{IgG}}{Immunoglobulin G levels}
    \item{\code{CD3}}{CD3 lymphocytes}
    \item{\code{CD19}}{CD19 lymphocytes}
    \item{\code{flu_post_vaccination_response}}{Antibody response to 'flu vaccination}
    \item{\code{tetanus_post_vaccination_response}}{Antibody response to tetanus vaccination}
  }
}
\source{
Pawlowski, Boguslaw et al. (2017), Data from: Body height and immune efficacy: testing body stature as a signal of biological quality, Dryad, Dataset, https://doi.org/10.5061/dryad.2vn0d
}
\references{
Drulis-Kawa, Z. (2017) Body height and immune efficacy: testing body stature as a signal of biological quality. Proceedings. Biological sciences / The Royal Society, 284: 0171372
}
\examples{
data(height_immunity)
str(height_immunity)
}
\keyword{datasets}
