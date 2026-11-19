\name{sarem_species}
\alias{sarem_species}
\title{Query and filter the CMA-SAREM mammal list}
\usage{
sarem_species(
  order = NULL,
  family = NULL,
  genus = NULL,
  species = NULL,
  category = NULL,
  environment = c("terrestrial", "marine", "all"),
  refresh = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{order}{Optional character vector of orders.}
\item{family}{Optional character vector of families.}
\item{genus}{Optional character vector of genera.}
\item{species}{Optional character vector of scientific names.}
\item{category}{Optional character vector of conservation categories.}
\item{environment}{One of \code{"terrestrial"}, \code{"marine"}, or \code{"all"}.}
\item{refresh}{Refresh the taxonomic lookup.}
\item{quiet}{Suppress progress messages.}
}
\value{A data frame with one row per species.}
\description{Queries the current CMA-SAREM species list and applies taxonomic,
conservation, and environment filters.}

