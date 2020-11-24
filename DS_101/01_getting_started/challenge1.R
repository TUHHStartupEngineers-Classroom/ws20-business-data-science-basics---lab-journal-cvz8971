# CHALLENGE 1
# load libraries ----
library(tidyverse)
library(readxl)

# import files ----
bikes_tbl      <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# examine data ----
#bikeshops_tbl
#glimpse(bikeshops_tbl)

# join data ----
# by automatically detecting a common column, if any ...
#left_join(orderlines_tbl, bikeshops_tbl)
# If the data has no common column name, you can provide each column name in the "by" argument. For example, by = c("a" = "b") will match x.a to y.b. The order of the columns has to match the order of the tibbles).
left_join(orderlines_tbl, bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
# Chaining commands with the pipe and assigning it to order_items_joined_tbl
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()
bike_orderlines_joined_tbl %>% glimpse()

# wrangling data ----
bike_orderlines_joined_tbl %>% 
  select(location) %>%
  filter(str_detect(location, "^Hamburg")) %>% 
  unique()
# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bikeshop_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # Separate category name
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  
  # Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # Optional: Reorganize. Using select to grab or remove unnecessary columns
  # by exact column name
  select(-...1, -gender) %>%
  
  # by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

glimpse(bike_orderlines_joined_tbl)
glimpse(bikeshop_orderlines_wrangled_tbl)

# Sales by Year ----
library(lubridate)
# Manipulate
sales_by_location_tbl <- bikeshop_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(state, total_price) %>%
  
  # Add year column
  mutate(state) %>%
  
  # Grouping by year and summarizing sales
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_location_tbl

# Visualize
#{r plot, fig.width=10, fig.height=7}
sales_by_location_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "In North Rhine-Westphalia most bikes has been sold",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# Sales by location and year ----

# Manipulate
sales_by_year_state_tbl <- bikeshop_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main category
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_state_tbl 

# Visualize
sales_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Most of the states have an upward trend",
    fill = "Sate" # Changes the legend name
  )


# Writing Files ----
# Excel
install.packages("writexl")
library("writexl")
bikeshop_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bikeshop_orderlines.xlsx")

# CSV
bikeshop_orderlines_wrangled_tbl %>% 
  write_csv("DS_101/00_data/01_bike_sales/02_wrangled_data/bikeshop_orderlines.csv")

# RDS
bikeshop_orderlines_wrangled_tbl %>% 
  write_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bikeshop_orderlines.rds")
