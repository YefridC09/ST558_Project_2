# The data selected for this project is the US Sales data

#First we are going to open the libraries that we are going to work with

#The data is saved in the `data/superstore_data.xls`

#We need the `readxl` package to red the file

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

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