# The data selected for this project is the US Sales data

#First we are going to open the libraries that we are going to work with

#The data is saved in the `data/superstore_data.xls`

#We need the `readxl` package to red the file

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(zipcodeR)
library(maps)

store_data <- read_excel("data/superstore_data.xls") |>
  clean_names() |>
  mutate(region = as.factor(region),
         category = as.factor(category),
         sub_category = as.factor(sub_category),
         state = as.factor(state),
         segment = as.factor(segment),
         ship_mode = as.factor(ship_mode),
         order_date = as_date(order_date),
         ship_date = as_date(ship_date))

store_data

#I am going to start making some one-way and two way contingency tables

# I this we will explore the segment to which the store sells the more of their products

one_cont_table1 <- table(store_data$segment)
  
one_cont_table1

# In this part I will check what is the count of sales per region 

one_cont_table2 <- table(store_data$region)

one_cont_table2

#We are going to create some two-way contingency tables

#The first will count the ship mode for each region

two_cont_table1 <- table(store_data$ship_mode, store_data$region)

two_cont_table1

# The second contingency table is for category per region

two_cont_table2 <- table(store_data$category, store_data$region)

two_cont_table2

# NOw we are going to create some numerical summaries for numeric variables at the level of the categorical variables.

#First we want to know what is the count,  mean and median of sales by state and we are going to sort it based on number of sales

summary_by_state <- store_data |>
  group_by(state) |>
  drop_na(state) |>
  summarize(mean_sales = mean(sales, na.rm = TRUE), 
            median_sales = median(sales, na.rm = TRUE),
            n_sales = n()) |>
  arrange(desc(n_sales))

summary_by_state

# We are going to generate numerical summaries for sales by region
summary_by_region <- store_data |>
  group_by(region) |>
  drop_na(region) |>
  summarize(mean_sales = mean(sales, na.rm = TRUE), 
            median_sales = median(sales, na.rm = TRUE),
            n_sales = n()) |>
  arrange(desc(n_sales))

summary_by_region

#Checking summaries of profit by region

summary_profit_region <- store_data |>
  group_by(region) |>
  drop_na(region) |>
  summarize(mean_profit = mean(profit, na.rm = TRUE), 
            median_profit = median(profit, na.rm = TRUE)) |>
  arrange(desc(median_profit))

summary_profit_region

#Summary to check what is the category that gets the higher discounts
summary_disc_cat <- store_data |>
  group_by(category, sub_category) |>
  drop_na(category, sub_category) |>
  summarize(mean_discount = mean(discount, na.rm = TRUE), 
            median_discount = median(discount, na.rm = TRUE)) |>
  arrange(desc(mean_discount))

summary_disc_cat

# We can continue with the graphs

#We are going to create a bar chart

bar_plot <- ggplot(data = store_data,
                   aes(x = category, fill = region)) + 
  geom_bar() +
  labs(title = "Number of sales by category, and region",
       x = "Category",
       y = "Count",
       fill = "Region") +
 # scale_colour_viridis_b()+
  theme_classic()

bar_plot

#now we create a boxplot for the sales variable, logarithmic scale is required for the data to be properly observed since there are very big sales compared to the median.

box_plot <-ggplot(data = store_data,
                   aes(x = sales, 
                       y = category)) + 
  geom_boxplot() +
  labs(title = "Box plot of sales by category, faceted by segment",
       x = "Sales",
       y = "Category") +
  scale_x_log10() +
  # scale_colour_viridis_b()+
  facet_wrap(~segment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

box_plot

#now we create a violin plot for the discount variable, trying to investigate the distribution of the values across all the sales, by category faceted by region

box_plot2 <-ggplot(data = store_data,
                  aes(x = discount, 
                      y = category)) + 
  geom_violin() +
  labs(title = "Box plot of discount by category, faceted by segment",
       x = "Discount",
       y = "Category") +
  facet_wrap(~region) +
  theme_bw()

box_plot2

#Now we create a scattered plot of sales vs profit colored by region and faceted by segment to investigate the relationship between sales and profit for each region, and segment.

sc_plot <- ggplot(data = store_data,
                  aes(x = sales, 
                      y = profit,
                      colour = region)) + 
  geom_point() +
  labs(title = "Profit vs sales, by  segment and region",
       x = "Sales",
       y = "Category",
       colour = "Region") +
  #scale_x_log10() +
  # scale_colour_viridis_b()+
  facet_wrap(~segment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

sc_plot

#We create a 100% stacked bar chart to show the proportion of bub categories that the most discounts.

sbar_plot2 <- ggplot(data = store_data,
                  aes(x = sub_category, 
                      fill = as.factor(discount))) + 
  geom_bar(position = "fill")+
  labs(title = "Profit vs sales, by  segment and region",
       x = "Sub category",
       y = "Proportion",
       fill = "Discount") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

sbar_plot2

#We are going to generate a map that shows the sales by zip code

#First we need to give the coordinates for each zip code to the table, so it match it with the sale per zip code

store_map <- store_data |>
  mutate(postal_code = as.character(postal_code)) |>
  left_join(
    zip_code_db |>
      select(zipcode, lat, lng),
    by = c("postal_code" = "zipcode"))

# Now we can proceed to generate the map

map_sales <- ggplot() +
     geom_polygon(
           data = us_map,
           aes(long, lat, group = group),
           fill = "gray",
           color = "white",
           linewidth = 0.2) +
     geom_point(data = store_map,
           aes(x = lng,
               y = lat,
               size = sales,
               color = sales),
           alpha = 0.7) +
  scale_color_viridis_c() +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "Sales by ZIP Code",
       x = NULL,
       y = NULL)

map_sales

#
