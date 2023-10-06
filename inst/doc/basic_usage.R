## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = nzchar(Sys.getenv("COMPILE_VIG"))
)

## ----setup,  message=FALSE, warning=FALSE-------------------------------------
library(cmhc)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
list_cmhc_tables() |>
  head() |>
  knitr::kable()

## -----------------------------------------------------------------------------
vacancy_data <- get_cmhc(survey="Rms",series="Vacancy Rate",dimension="Bedroom Type",
                         breakdown="Historical Time Periods",  geo_uid="3506008")

ggplot(vacancy_data,aes(x=Date,y=Value/100,colour=`Bedroom Type`)) +
  geom_point(aes(shape=Quality), data=~filter(.,!is.na(Value))) +
  geom_line() +
  scale_y_continuous(labels=scales::percent) +
  labs(title="City of Ottawa rental vacancy rate",
       x=NULL, y="Rental vacancy rate",
       caption="CMHC Rms")

## -----------------------------------------------------------------------------
list_cmhc_filters("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods")$Filters

## -----------------------------------------------------------------------------
vacancy_data2 <- get_cmhc(survey="Rms",series="Vacancy Rate",dimension="Bedroom Type",
                         breakdown="Historical Time Periods",  geo_uid="3506008",filters = list("season"="April"))

bind_rows(vacancy_data,vacancy_data2) |>
  filter(!is.na(Value)) |>
  ggplot(aes(x=Date,y=Value/100,colour=`Bedroom Type`)) +
  geom_point(aes(shape=Quality), data=~filter(.,!is.na(Value))) +
  geom_line() +
  scale_y_continuous(labels=scales::percent) +
  labs(title="City of Ottawa rental vacancy rate",
       x=NULL, y="Rental vacancy rate",
       caption="CMHC Rms")

## -----------------------------------------------------------------------------
availability_rate <- c("April","October") |>
  lapply(function(season)
    get_cmhc(survey="Rms",series="Availability Rate",dimension="Bedroom Type",
             breakdown="Historical Time Periods",  geo_uid="3506008",filters = list("season"=season))) |>
  bind_rows()

availability_rate |>
  filter(!is.na(Value)) |>
  ggplot(aes(x=Date,y=Value/100,colour=`Bedroom Type`)) +
  geom_point(aes(shape=Quality), data=~filter(.,!is.na(Value))) +
  geom_line() +
  scale_y_continuous(labels=scales::percent) +
  labs(title="City of Ottawa rental availability rate",
       x=NULL, y="Rental availability rate",
       caption="CMHC Rms")

## -----------------------------------------------------------------------------
completions_data <- get_cmhc(survey="Scss",series="Completions",dimension="Dwelling Type",
                             breakdown="Historical Time Periods", geo_uid="5915022",
                             frequency = "Annual")

ggplot(completions_data,aes(x=Date,y=Value,fill=`Dwelling Type`)) +
  geom_bar(stat="identity") +
  facet_wrap(~`Dwelling Type`,scales="free_y") +
  scale_fill_discrete(guide="none") +
  scale_y_continuous(labels=scales::comma) +
  labs(title="City of Vancouver housing completions",
       x=NULL, y="Annual completions",
       caption="CMHC Scss")

## -----------------------------------------------------------------------------
under_construction_data <- get_cmhc(survey="Scss",series="Under Construction",dimension="Dwelling Type",
                                    breakdown="Census Tracts", geo_uid="5915022",
                                    year = 2022, month = 4)

geo_data <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format = 'sf', level="CT", quiet=TRUE)

geo_data |>
  left_join(under_construction_data |> 
              filter(`Dwelling Type`=="All"),
            by="GeoUID") |>
  mutate(value=cut(coalesce(Value,0),
                   breaks=c(-Inf,0,10,50,100,500,1000,1500,Inf),
                   labels=c("0","1 to 10","11 to 50","51 to 100","101 to 500",
                            "501 to 1000","1001 to 1500","Over 1500"))) |>
  ggplot(aes(fill=value)) +
  geom_sf() +
  scale_fill_viridis_d() +
  coord_sf(datum=NA) +
  labs(title="City of Vancouver housing units under construction April 2022",
       fill="Number of units",
       caption="CMCH Scss")

