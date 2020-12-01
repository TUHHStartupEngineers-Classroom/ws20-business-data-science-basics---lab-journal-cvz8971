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
# 1. Basic column operations ----

bikes_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# three different ways to select the first 3 columns
bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))

# reduce columns
bikes_tbl %>%
  select(model, price)

# rearrange columns: Put the category columns in front (select_helpers):
bikes_tbl %>%
  select(category_1:category_3, everything())

# alternative using relocate()
bikes_tbl %>%
  relocate(category_1:category_3)

# Select helpers: Select all columns that start with model:
bikes_tbl %>%
  select(starts_with("model"))

# Pull() extracts content of a tibble column. Calculate the mean of price
bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()


# First extract all character columns. Then extract all non numeric columns.
bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))


# Use rename() to rename one column at a time.
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

# Use set_names() to rename all columns at once.
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

# An example using str_replace
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())


# 2. Basic row operations ----

# Select model and price and arrange the data by price in a descending order
bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

# Filter rows, where price is greater than the mean of price
bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))


# Filter rows, where price is greater 5000 or lower 1000 and sort by descending price
bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()


# Filter rows, where price is greater 5000 and the model contains “Endurace” (str_detect())
bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )


# Filter rows, where the category_1 is “Hybrid / City” or “E-Bikes”. Use the %in% operator
bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))


# Filter rows, where the category_2 is “E-Mountain”
bikes_tbl %>%
  filter(category_2 == "E-Mountain")


# negate the two previous ones
bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))


# Filtering rows with row number(s) using slice():
# Arrange by price (1. ascending and 2. descending) and filter the first 5 rows
bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

#Arrange by price (descending) and filter the last 5 rows (use nrow())
bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))


# List unique values for category_1, for a combination of category_1 and category_2 and for combination of category_1, category_2 and category_3
bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)


# 3. Column transformations ----
bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# Add the column “freight_costs”. The costs are 2 € per kilogram
bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight)

# Overwrite Columns. Replace total_price with the log values of it
bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))


# Transformations: Add the log and the square root of the total_price
bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5)


# Adding Flags (feature engineering): Add a column that equals to TRUE if model contains the word “strive” and filter by that
bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)

# Binning with ntile(): Add a column and create 3 groups for total_price, where the groups each have as close to the same number of members as possible
bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())

# More flexible binning with case_when(): Numeric to categorical. 
# Add a column, use case_when and choose the quantiles yourself (use quantile()). Set the results to High, Medium and Low
bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything())


# More flexible binning with case_when(): Text to categorical. 
# Add a column that equals to “Aeroad”, when model contains “aerorad”, “Ultimate”, when model contains “ultimate” and “Not Aeroad or Ultimate” in every other case
bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything())


# 4. Summary calculations ----

# Summarize the total revenue
bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )


# Summarize the total revenue for each category_1
bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))


# Summarize the total revenue for the groups made of category_1 and category_2. Sort by descending revenue
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise(). Left-over groups will cause difficult-to-detect errors.
  ungroup() %>%
  arrange(desc(revenue))

# Summary functions: Group by category_1 and category_2 and summarize the price by
#count
#average
#median
#standard deviation
#minimum
#maximum

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg   = mean(total_price),
    med   = median(total_price),
    sd    = sd(total_price),
    min   = min(total_price),
    max   = max(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(count))



# Create total_price column and insert missing values for demonstration
bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

# detect missing (absolute)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))))

# detect missing (relative)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.)))

# Handling missing data
bike_orderlines_missing %>%
  filter(!is.na(total_price))


# 5. Reshaping/Pivoting ----

# pivot_wider() makes the values of the column “size” to columns
# Create a tibble with the sales for each category_1 and each bikeshop
bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))


# Make the values of category_1 to columns and make the values to a euro format 
bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )



# Wide to Long. Recreate the original tibble from bikeshop_revenue_formatted_tbl
bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())


# 6. Joining & Binding ----

# Create two tibbles
order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

# Bind them back together using left_join()
order_dates_tbl %>%
  
  # By argument not necessary, because both tibbles share the same column names
  left_join(y = order_items_tbl, by = c("order_id" = "order_id", "order_line" = "order_line"))


# bind_cols(): Remove all columns from bike_orderlines_tbl that contain “category” but bind back the column category_1
bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )


#bind_rows(): Can be useful for splitting a dataset into a training and a test dataset.

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

#Bind them back together using bind_rows().
train_tbl %>%
  bind_rows(test_tbl)

# 7. Splitting & Combining ----

# Select order_date and convert it to character. 
# Then separate it into year, month and day. Make each column numeric. 
# Combine them again using unite() and convert the column back to a Date format (as.Date())
bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  
  # unite
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))



url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)
covid_data_dt[year == 2019, sum(cases), by = continentExp]
covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt$dateRep 
covid_data_dt[1:2]
# Sort covid_data_dt first by column year, month and day in ascending order, and then by countriesAndTerritories in descending order
covid_data_dt[order(year, month, day, -countriesAndTerritories)]

# Return as a vector
covid_data_dt[,geoId]
# Select multiple columns
covid_data_dt[,c("geoId", "countriesAndTerritories")]

# Return as a data.table
covid_data_dt[,list(geoId)]
# Short form using .
covid_data_dt[,.(geoId)]
# Select multiple columns
covid_data_dt[,.(geoId, countriesAndTerritories)]

# Rename them directly
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

# select columns named in a variable using the ..prefix
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

# List names 
colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

## Exercise 1 ----
# Convert the in-built airquality dataset to a data.table. 
# Then select Solar.R, Wind and Temp for those rows where Ozone is not missing.
data("airquality")
# Solution 1
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

# Solution 2
setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]


# How many days have had more than 1000 deaths in a country
covid_data_dt[,sum(deaths > 1000)]
# to list the observations put it in i
covid_data_dt[deaths > 1000]

covid_data_dt[, deaths_per_capita := deaths / popData2019]
covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]
# modify existing columns:
covid_data_dt[,date := lubridate::dmy(date)]

## Exercise 2 ----
# Convert the in-built mtcars dataset to a data.table. 
# Create a new column called mileage_type that has the value high if mpg > 20 else has value low.
data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]






# 2.0 DATA IMPORT ----

# 2.1 Loan Acquisitions Data ----
# specify the datatype of each column, that we want to import

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))


# import the data
acquisition_data <- vroom(
  file       = "loan_data/Acquisition_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_acq),
  col_types  = col_types_acq,
  na         = c("", "NA", "NULL"))
acquisition_data %>% glimpse()


# 2.2 Performance Data ----
col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))

performance_data %>% glimpse()


# 3.1 Acquisition Data ----
class(acquisition_data)

setDT(acquisition_data)

class(acquisition_data)

acquisition_data %>% glimpse()

# 3.2 Performance Data ----
setDT(performance_data)

performance_data %>% glimpse()



# 4.0 DATA WRANGLING ----

# 4.1 Joining / Merging Data ----

tic()
combined_data <- merge(x = acquisition_data, y = performance_data, 
                       by    = "loan_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_data %>% glimpse()

# Same operation with dplyr
tic()
performance_data %>%
  left_join(acquisition_data, by = "loan_id")
toc()


# Preparing the Data Table

setkey(combined_data, "loan_id")
key(combined_data)

?setorder()
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))


# 4.3 Select Columns ----
combined_data %>% dim()

keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")


combined_data <- combined_data[, ..keep_cols]

combined_data %>% dim()

combined_data %>% glimpse()



# 4.4 Grouped Mutations ----
# - Add response variable (Predict wether loan will become delinquent in next 3 months)

# dplyr
tic()
temp <- combined_data %>%
  group_by(loan_id) %>%
  mutate(gt_1mo_behind_in_3mo_dplyr = lead(current_loan_delinquency_status, n = 3) >= 1) %>%
  ungroup()  
toc()

combined_data %>% dim()
temp %>% dim()

# data.table
tic()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
toc()

combined_data %>% dim()

# Remove the temp variable
rm(temp)

