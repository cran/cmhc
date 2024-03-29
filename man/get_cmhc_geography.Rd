% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmhc_geography.R
\name{get_cmhc_geography}
\alias{get_cmhc_geography}
\title{Get CMHC geographies for CMHC Survey Zones and Neighbourhoods}
\usage{
get_cmhc_geography(
  level = c("CT", "ZONE", "NBHD", "CSD", "MET"),
  base_directory = Sys.getenv("CMHC_CACHE_PATH")
)
}
\arguments{
\item{level}{aggregation level for geographic data, one of "CT","ZONE","NBHD","CSD","MET"}

\item{base_directory}{local directory to hold CMHC geography data, by default this is inferred from the
CMHC_CACHE_PATH environment variable. To use this function a local data directory has to be set.}
}
\value{
A spatial data frame with the geographies for the specified geographic level.
}
\description{
The data can be queried for Census Tracts, Survey Zones, Neighbourhoods, Census Subdivisions and Metropolitan Areas,
but it's most useful for Survey Zones, Neighbourhoods which are particular to CMHC and not available from other sources.
The geographic data corresponds to an extract from 2017, and won't necessarily match regions from other years. The Survey Zones
and Neighbourhoods have been quite stable, but census geographies change over time and can be matched with geographic
data obtained by using the `cancensus` package.

The geographic data is quite large and a local cache directory needs to be provided. By default the
"CMHC_CACHE_PATH" environment variable is used to determine the cache directory, it can be set via the
`set_cache_path` function. The geographic data will take up about 55Mb of disk space.
}
\examples{
\dontrun{
get_cmhc_geography("ZONE")
}

}
