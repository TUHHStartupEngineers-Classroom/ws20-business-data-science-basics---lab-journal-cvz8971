# CHALLENGE 3: DATA WRANGLING
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
  "tictoc"      # counter
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

# load the data ----

## patent ----
col_types_patent <- list(
  id = col_character(),
  type = col_skip(), #col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(), #col_character(),
  title = col_skip(), #col_character(),
  kind = col_skip(), #col_character(),
  num_claims = col_double(),
  filename = col_skip(), #col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)
patent_tbl %>% glimpse()

## assignee ----
col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_skip(), #col_character(),
  name_last = col_skip(), #col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "patent.tsv/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)
assignee_tbl %>% glimpse()

## patent_assignee ----
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "patent.tsv/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl %>% glimpse()

## uspc ----
col_types_uspc <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "patent.tsv/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)
uspc_tbl %>% glimpse()


setDT(patent_tbl)
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)
patent_tbl %>% glimpse()
patent_assignee_tbl %>% glimpse()
assignee_tbl %>% glimpse()
uspc_tbl %>% glimpse()

# 1. Patent dominance ----
# The 10 US companies with the most assigned/granted patents.

a_p_a_combined <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by.x  = "id", 
                       by.y  = "assignee_id",
                       all.x = TRUE, 
                       all.y = FALSE)

a_p_a_combined %>% glimpse()


setkey(a_p_a_combined, "id")
#key(a_p_a_combined)
setorderv(a_p_a_combined, c("id", "organization"))

a_p_a_combined %>% dim()
#a_p_a_combined$id %>% unique()
a_p_a_combined %>% glimpse()

us_patents <- a_p_a_combined %>%
  select(1:3) %>%
  filter(type == "2")
  
us_patents %>% glimpse()

us_patents %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)

# 2. Recent patent activity ----
# top 10 US companies with the most new granted patents for 2019

a_p_a_p_combined <- merge(x = patent_tbl, y = a_p_a_combined, 
                        by.x    = "id",
                        by.y = "patent_id",
                        all.x = TRUE, 
                        all.y = FALSE)

a_p_a_p_combined %>% glimpse()
setkey(a_p_a_p_combined, "id")
#key(a_p_a_p_combined)
#a_p_a_p_combined$withdrawn %>% unique()
setorderv(a_p_a_p_combined, c("id", "organization"))

us_patents2019 <- a_p_a_p_combined %>%
  filter(type == "2") %>%
  filter(country == "US") %>%
  filter(date >= "2019-01-01" & date <="2019-12-01") %>%
  filter(withdrawn != 1)  ####################################### richtig ?
us_patents2019 %>% glimpse()

us_patents2019 %>%
#  filter(organization != na) %>%
  group_by(organization) %>%   
  summarise(
    count = n()                       ####################### #* num_claims ?
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)

# 3. Innovation in Tech ----

tech_patents <- merge(x = uspc_tbl, y = a_p_a_combined, 
                          by = "patent_id",
                          all.x = TRUE, 
                          all.y = FALSE)
patents %>% glimpse()

setkey(tech_patents, "patent_id")
#key(a_p_a_combined)
setorderv(tech_patents, c("patent_id", "organization"))


tech_patents %>%
  select(1:3) %>%
  filter(type == "2" | type == "3")

us_patents %>% glimpse()

us_patents %>%
  group_by(organization) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  slice(1:10)





us_tblf <- uspc_tbl %>%
  select(sequence, mainclass_id, patent_id ) %>%
  #filter(sequence == 0) %>%
  left_join(patent_assignee_tbl, by = "patent_id")#%>%
  #left_join(assignee_tbl, by = "id") %>%
  #select(patent_id, organization, mainclass_id) 
us_tblf %>% glimpse()

