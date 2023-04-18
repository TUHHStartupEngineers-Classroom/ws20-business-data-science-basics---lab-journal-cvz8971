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

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

#scales::dollar(100, prefix = "", suffix = " €",
#               big.mark = ".", decimal.mark = ",")

#bike_orderlines_tbl <- read_rds("DS_101/02_data_wrangling/bike_orderlines.rds")
bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

bike_orderlines_tbl %>% glimpse()

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # € Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " €"))
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
# with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )




# Themes

library(ggthemes)
## DATA PREPARATION
sales_by_month_2015 <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a month column
  select(order_date, total_price) %>%
  mutate(year  = year(order_date)) %>% 
  mutate(month = month(order_date)) %>%
  
  filter(year == "2015") %>%
  
  # Grouping by month, and summarizing sales
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # $ Format Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",  
                                     suffix          = " €"))

## PLOTTING
# Canvas
sales_by_month_2015 %>% 
  ggplot(aes(x = month, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # Formatting
  expand_limits(y = 0) +
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month, 
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M")) +
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers are dropping towards the end of the year."
  )  +  
  theme_economist() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))





# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)


# colors 
# Named Colors. This returns a long list of colors that can be used by name
colors()

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "slateblue")

# To RGB
col2rgb("slateblue")

col2rgb("#2C3E50")

# To HEX (this function should be provided to a geom)
rgb(44, 62, 80, maxColorValue = 255)



## Brewer. Comes with basic R.
#Primarly for discrete data.

# We can use those palletes by just calling their names (e.g. "Blues")
# Display the colors
RColorBrewer::display.brewer.all() 
# Get information
RColorBrewer::brewer.pal.info
# Get the HEX codes
RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])


### Viridis
viridisLite::viridis(n = 20)
# The last two characters indicate the transparency (e.g. FF makes it 100% transparent)

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = viridisLite::viridis(n = 20)[10])