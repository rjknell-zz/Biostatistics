\name{worldbank}
\alias{worldbank}
\docType{data}
\title{
Data comparing 186 countries originally published by the World Bank
}
\description{
    A variety of geographical, economic, environmental
    and social measures for 186 countried from the year 
    2014. Compiled from data published by the World Bank.
}
\usage{data("worldbank")}
\format{
  A data frame with 186 observations on the following 25 variables.
  \describe{
    \item{\code{Climate_region}}{a factor with levels \code{Temperate or Polar} \code{Tropical}}
    \item{\code{Income_binary}}{a factor with levels \code{High} \code{Low}}
    \item{\code{Country_Name}}{Name of the country}
    \item{\code{Country_Code}}{Three letter code for the country}
    \item{\code{Region}}{Geographic region}
    \item{\code{Income_group}}{Divides countries into one of four income groups}
    \item{\code{Population}}{Population size}
    \item{\code{Land_area}}{Area of the country in km^2}
    \item{\code{Forest_area}}{Area forested as percent of land area}
    \item{\code{Precipitation}}{Annual precipitation in mm}
    \item{\code{Population_density}}{People per km^2}
    \item{\code{Capital_lat}}{Latutude of the capital}
    \item{\code{GNP_per_Cap}}{Gross National Product per capita in US$}
    \item{\code{Population_growth}}{Annual population growth in percent}
    \item{\code{Cereal_yield}}{Cereal yield in Kg per Ha}
    \item{\code{Female_life_expectancy}}{Average life expectancy of women in years}
    \item{\code{Under_5_mortality}}{Deaths of children under 5 per 100000}
    \item{\code{Renewables.}}{Renewable energy consumption (percent of total final energy consumption)}
    \item{\code{CO2}}{CO2 production in tonnes per capita}
    \item{\code{PM25}}{PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)}
    \item{\code{Women_in_parliament}}{Percent of seats held by women in national parliaments}
    \item{\code{GINI_index}}{Gini index of wealth inequality}
    \item{\code{Govt_spend_education}}{Government expenditure on education, total (percent of GDP)}
    \item{\code{Secondary_school_enrolment}}{School enrollment, secondary (percent net)}
    \item{\code{School_gender_parity}}{Index of gender parity for school enrollment}
  }
}
\source{
https://data.worldbank.org/indicator
}
\examples{
data(worldbank)
str(worldbank)
}
\keyword{datasets}
