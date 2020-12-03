# CHALLENGE 2.2: WEBSCRAPING 
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
  "furrr"       # parallel Processing
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

# 3. Web scraping

# API ----

install.packages("mapsapi")
install.packages("leaflet")

library(mapsapi)
library(leaflet)

#API Parameter
key <- "AIzaSyBrRUkv83znL_giJRlEpTq03oN8IaXV-rM"
url <- "https://maps.googleapis.com/maps/api/directions/json?"
modes <- c("driving","transit", "walking", "bicycling")
tra_model <- c("best_guess", "pessimistic", "optimistic")
avoid = c(NA, "tolls", "highways", "ferries", "indoor")
dep_time = Sys.time() + as.difftime(1, units = "hours")

# Get directions from the Google Maps Directions API
route = mp_directions(
  origin = "Schoene Aussicht Hamburg",
  destination = "Elbphilharmonie Hamburg",
  #departure_time = dep_time,
  mode = modes[[1]],  
  #traffic_model = tra_model[[1]],
  alternatives = FALSE,
  key = key,
  quiet = TRUE
)
route_data = mp_get_routes(route)

# visualization
pal = colorFactor(palette = "Dark2", domain = route_data$alternative_id)
leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(data = route_data, opacity = 0.9, weight = 6, color = ~pal(alternative_id))


# Separate segments can be extracted from the same response 
route_seg = mp_get_segments(route)
head(route_seg)

pal = colorFactor(
  palette = sample(colors(), length(unique(seg$segment_id))), 
  domain = route_seg$segment_id
)

leaflet(route_seg) %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolylines(opacity = 1, weight = 7, color = ~pal(segment_id), popup = ~instructions)



# Web scraping ----
## 3.1.1 PRODUCT FAMILIES ----


#xopen(url_home) # Open links directly from RStudio to inspect them

bike_family_tbl <- read_html("https://www.rosebikes.com/bikes") %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-navigation__link") %>%
  # ...and extract the information of the id attribute
  html_attr('title') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  #discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl

## 3.1.2 PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
family_id_css

# Extract the urls from the href attribute
bike_category_tbl <- read_html("https://www.rosebikes.com/bikes/mtb") %>%
  
  # Select nodes by the ids
  #html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".catalog-category-bikes__picture-wrapper") %>% #"catalog-navigation__list-item") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

bike_category_tbl

bike_prices_tbl <-read_html("https://www.rosebikes.com/bikes/mtb") %>%
  html_nodes(css = ".catalog-category-bikes__price-title") %>% #"catalog-navigation__list-item") %>%
  #str_remove(pattern = "\\?.*") %>%
  html_text() %>%
  stringr::str_extract("(?<=from ).*")

bike_prices_tbl 





get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)

  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-model > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
}

# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)
bike_data_lst

bike_data_lst[[4]] <- bike_category_tbl$url[4]   #kein Preis
bike_data_lst[[5]] <- bike_category_tbl$url[5]   #kein Preis
bike_data_lst[[8]] <- bike_category_tbl$url[8]

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)
bike_data_tbl
saveRDS(bike_data_tbl, "bike_data_tbl.rds")
