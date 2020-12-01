# CHALLENGE 4: VISUALIZATION
# packages ----
pkgs_cran <- c(
  "fs",         # working with the file system
  "readxl",     # reading excel files
  "writexl",    # saving data as excel files
  "tidyverse",  # dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats
  "lubridate",  # working with dates and times
  "devtools",   # used to install non-CRAN packages
  "RSQLite",    # to open up a connection to the database
  "dplyr",
  "DBI",
  "httr",       # for http requests
  "glue",       # concatenation and interpolation of strings
  "jsonlite",   # JSON structure <-> character format
  "rstudioapi", # credentials
  "rvest",      # take HTML and CSS selectors of webpages for finding the relevant fields which contain the desired information
  "stringr",    # for data cleaning and preparation 
  "stringi",    # character string/ text processing
  "xopen",      # quickly opening URLs
  "purrr",      # suite of functions for iteration and functional programming
  "furrr",      # parallel Processing
  "data.table", # alternative to default data.frame or tibble from the tidyverse to handle tabular data
  "vroom",      # fast reading in of delimited files
  "tictoc",     # counter
  "ggmap"        # for map_data 
)
install.packages(pkgs_cran)  

# libraries ----
library("writexl")
library("fs")
library("devtools")
library(tidyverse)
library(readxl)
library(lubridate)
library("RSQLite")
library("dplyr")
library("DBI")
library(httr)
library(glue)
library(jsonlite)
library("rstudioapi")
library(rvest)
library("stringr")
library(xopen)
library(stringi)
library(furrr)
library(data.table)
library(vroom)
library(tictoc)
library(maps)
library(ggmap)

# 1. time course of the cumulative Covid-19 cases -----
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_tbl %>% glimpse()

covid_data2020_tbl <- covid_data_tbl %>%
  select(1:5, 7, 11, 12) %>%
  filter(year == 2020)

covid_data2020_tbl %>% glimpse()
covid_data2020_tbl$countriesAndTerritories %>% unique()

covid_data2020_tbl %>%
  #group_by(continentExp) %>%
  #summarize(continentCases = sum(cases)) %>%
  #ungroup() %>%
  filter(countriesAndTerritories %in% c("Germany", "France","United_Kingdom","Spain", "United_States_of_America")) %>%

  ggplot(aes(month, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)) + 
  geom_line(aes(color = countriesAndTerritories), size = 1) + 
  scale_x_continuous(breaks = 1:11, labels =c('January', 'February','March', 'April','May', 'June','July', 'August','September', 'October','November')) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-2, 
                                             prefix = "",
                                             suffix = "M")) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/02/2020, Europe had more cases than the USA",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Continent / Country"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
   # legend.direction = "vertical",
    plot.title = element_text(face = "bold"),
  ) +
  coord_cartesian(ylim = c(0, 1000))
  
covid_data2020_tbl$countriesAndTerritories %>% unique()
covid_data2020_tbl$continentExp %>% unique()      # Europe"





# 2. distribution of the mortality rate ----

covid_dataworld_tbl <- covid_data_tbl %>%
  select(1:4, 6:10) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  group_by(countriesAndTerritories) %>%
  summarize(countryDeaths = sum(deaths)) %>%
  ungroup()

covid_deaths_tbl <- covid_dataworld_tbl %>%
  left_join(covid_data_tbl) %>%
  select(1,2,11) %>%
  group_by(countriesAndTerritories) %>%
  distinct() %>%
  mutate(Mortality_Rate = countryDeaths / popData2019) %>%
  select(1,4) 

covid_deaths_tbl %>% glimpse()
covid_dataworld_tbl %>% glimpse()
world <- map_data("world")
world %>% glimpse()

covid_deaths_tbl %>%
  ggplot() +
  geom_map(data = covid_deaths_tbl,
           map = world,
           aes(map_id = countriesAndTerritories, fill= Mortality_Rate),
           size=0.15,
           colour="darkred" ) +
  scale_y_continuous(labels = scales::percent) +
  #scale_color_continuous(low    = "#95E1EA", high = "#1097A3") +
  expand_limits(x = world$long, y = world$lat)+
  coord_fixed() +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold")) +
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    caption = "Date: 12/01/2020",
    color = "Mortality Rate"
  )
