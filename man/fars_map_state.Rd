% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars.R
\name{fars_map_state}
\alias{fars_map_state}
\title{fars_map_state
fars_map_state Read the data for a specific state and year and plot it in a map of the state with the location}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The integer number refering to the state we want to plot the information}

\item{year}{Year from which we want to extract the information. Not a range!}
}
\value{
A plot of the state with the location of the data, as long as the state is valid and there is data to plot
}
\description{
fars_map_state
fars_map_state Read the data for a specific state and year and plot it in a map of the state with the location
}
\examples{
\dontrun{
fars_map_state(6,2014)
}

}
