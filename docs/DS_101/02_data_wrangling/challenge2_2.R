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
bike_category_tbl <- read_html("https://www.rosebikes.de/fahrr%C3%A4der/mtb") %>%
  
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
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

bike_category_tbl

