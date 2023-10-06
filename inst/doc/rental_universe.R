## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = nzchar(Sys.getenv("COMPILE_VIG"))
)

## ----setup--------------------------------------------------------------------
library(cmhc)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
rental_data <- seq(1990,2021) |>
  lapply(function(y)get_cmhc("Rms","Rental Universe","Year of Construction","Census Subdivision",
                             geo_uid="59933",year=y)) %>%
  bind_rows() 

## -----------------------------------------------------------------------------
rental_data %>%
  filter(!is.na(`Census Subdivision`)) %>%
  mutate(Name=gsub("\\(C\\)","(CY)",`Census Subdivision`)) %>%
  mutate(Name=recode(Name,
                     "Burnaby (DM)"="Burnaby (CY)",
                     "Coquitlam (DM)"="Coquitlam (CY)",
                     "Surrey (DM)"="Surrey (CY)")) %>%
  filter(Name %in% (filter(.,`Year of Construction`=="Total",Date==max(Date)) %>%
           slice_max(Value,n=6) %>%
           pull(Name))) %>%
  filter(`Year of Construction`!="Total") %>%
  group_by(Name,`Year of Construction`) %>%
  mutate(Change=Value-lag(Value,order_by = Date)) %>%
  mutate(`Year of Construction`=factor(`Year of Construction`,levels=rev(levels(`Year of Construction`)))) %>%
  ungroup() %>%
  filter(Date!=min(Date)) %>%
  ggplot(aes(x=Date,y=Change,fill=`Year of Construction`)) +
  geom_bar(stat="identity") +
  facet_wrap(~Name,scales="free_y") +
  theme(legend.position = "bottom") +
  labs(title="Year over year change in rental stock",
       y="Number of units",
       x=NULL,
       fill="Period of construction",
       caption="CMHC Rms")

## ----fig.width=8.5------------------------------------------------------------
rental_data %>%
  filter(!is.na(`Census Subdivision`)) %>%
  mutate(Name=gsub("\\(C\\)","(CY)",`Census Subdivision`)) %>%
  mutate(Name=recode(Name,
                     "Burnaby (DM)"="Burnaby (CY)",
                     "Coquitlam (DM)"="Coquitlam (CY)",
                     "Surrey (DM)"="Surrey (CY)")) %>%
  filter(Name %in% (filter(.,`Year of Construction`=="Total",Date==max(Date)) %>%
           slice_max(Value,n=20) %>%
           pull(Name))) %>%
  filter(Date==max(Date)|Date==min(Date)) %>%
  group_by(Name,`Year of Construction`) %>%
  mutate(Change=coalesce(Value,0)-coalesce(lag(Value,order_by = Date),0)) %>%
  ungroup() %>%
  filter(Date==max(Date)) %>%
  mutate(Name=factor(Name,levels=filter(.,`Year of Construction`=="Total") |> arrange(Change) |> pull(Name))) %>%
  mutate(`Year of Construction`=factor(`Year of Construction`,levels=rev(levels(`Year of Construction`)))) %>%
  ggplot(aes(y=Name,x=Change,fill=`Year of Construction`)) +
  geom_bar(data=~filter(.,`Year of Construction`!="Total"), stat="identity") +
  geom_point(data=~filter(.,`Year of Construction`=="Total"), 
             aes(colour=`Year of Construction`),fill="black") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values=c("Total"="black")) +
  scale_x_continuous(labels=scales::comma) +
  labs(title="Change in rental stock 1990-2021",
       x="Number of units",
       y=NULL,colour=NULL,
       fill="Period of construction",
       caption="CMHC Rms")

## -----------------------------------------------------------------------------
rental_data <- c(1990,2021) |>
  lapply(function(y)get_cmhc("Rms","Rental Universe","Bedroom Type","Census Subdivision",
                             geo_uid="59933",year=y)) %>%
  bind_rows() 

rental_data %>%
  filter(!is.na(`Census Subdivision`)) %>%
  mutate(Name=gsub("\\(C\\)","(CY)",`Census Subdivision`)) %>%
  mutate(Name=recode(Name,
                     "Burnaby (DM)"="Burnaby (CY)",
                     "Coquitlam (DM)"="Coquitlam (CY)",
                     "Surrey (DM)"="Surrey (CY)")) %>%
  filter(Name %in% (filter(.,`Bedroom Type`=="Total",Date==max(Date)) %>%
           slice_max(Value,n=20) %>%
           pull(Name))) %>%
  filter(Date==max(Date)|Date==min(Date)) %>%
  group_by(Name,`Bedroom Type`) %>%
  mutate(Change=coalesce(Value,0)-coalesce(lag(Value,order_by = Date),0)) %>%
  ungroup() %>%
  filter(Date==max(Date)) %>%
  mutate(Name=factor(Name,levels=filter(.,`Bedroom Type`=="Total") |> arrange(Change) |> pull(Name))) %>%
  ggplot(aes(y=Name,x=Change,fill=`Bedroom Type`)) +
  geom_bar(data=~filter(.,`Bedroom Type`!="Total"), stat="identity") +
  geom_point(data=~filter(.,`Bedroom Type`=="Total"), 
             aes(colour=`Bedroom Type`),fill="black") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values=c("Total"="black")) +
  scale_x_continuous(labels=scales::comma) +
  labs(title="Change in rental stock 1990-2021",
       x="Number of units",
       y=NULL,colour=NULL,
       fill="Bedroom Type",
       caption="CMHC Rms")

