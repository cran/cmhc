% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmhc.R
\name{get_cmhc}
\alias{get_cmhc}
\title{Access CMHC data via the HMIP.}
\usage{
get_cmhc(
  survey,
  series,
  dimension,
  breakdown,
  geoFilter = "Default",
  geo_uid,
  year = NULL,
  quarter = NULL,
  month = NULL,
  frequency = NULL,
  filters = list(),
  refresh = FALSE
)
}
\arguments{
\item{survey}{The CMHC survey, one of "Scss", "Rms", "Srms", and "Seniors",  consult `list_cmhc_surveys()` for possible values.
(Other surveys and more data series may be supported in future versions.)}

\item{series}{The CMHC data series of the survey, consult `list_cmhc_series()` for possible values.}

\item{dimension}{The dimension to show in the results, consult `list_cmhc_dimensions()` for possible values.}

\item{breakdown}{The geographic breakdown, one of "Survey Zones", "Census Subdivision", "Neighbourhoods", "Census Tracts",
if querying data for a snapshot in time or "Historical Time Periods" if querying time series data. Not all geographic breakdowns are available for all series.
returns data frame with CMHC data, tile and subtitle are set as attributes.  Consult `list_cmhc_breakdowns()` for possible values.}

\item{geoFilter}{optional geographic type, only relevaent for provincial and Canada-wide tables. Options are "Default" (the default)
which considers accesses the default tables which should be used for data at the metro area or finer geographies. The other designaters
are only useful when used in conjunction with `geo_uid`s for provinces or all of Canada. Specifying "All"  will
give data for all sub-regions, "Metro", which only considers data in metro areas, "50k" and "10k", which only considers data from metro areas and agglomerations with at least
50k or 10k people, respectively.}

\item{geo_uid}{Census geographic identifier for which to query the data. Can be a census tract, census subdivision,
or census metropolitan area.}

\item{year}{optional, only needed when querying data for a snapshot in time.}

\item{quarter}{optional, only needed when querying data for a snapshot in time querying quarterly data.}

\item{month}{optional, only needed when querying data for a snapshot in time.}

\item{frequency}{optional, only needed when querying time series data, one of "Monthly", "Quarterly" or "Annual".}

\item{filters}{optional list of filters, consult `list_cmhc_filters()` for possible values.}

\item{refresh}{optional, refresh the temporary local cache of the CMHC tables. Defaults to `FALSE`.}
}
\value{
A tibble with the data in long form.
}
\description{
The data access needs to specify the survey, series, dimension (if any), and breakdown to specify the
CMHC table to pull the data from. The `list_cmhc_tables()` function can be used to list all the tables available
via this package. Snapshot data needs to specify the year, or if it is monthly data the month. Time series data, i.e.
when  `breakdown="Historical Time Series` is specified, does not need year or month parameters, but may have the
frequency parameter set. Filters provide additional ways to filter the tables by sub-categories.
}
\examples{
\dontrun{
data <- get_cmhc("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods","5915022")
}
}
\keyword{API}
\keyword{CMHC}
\keyword{Canada}
\keyword{data}
