
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(zipcodeR)
library(maps)

#First we are going to import the data, which is going to be run one time when the app is launched
store_data <- read_excel("../data/superstore_data.xls") |>
  clean_names() |>
  mutate(region = as.factor(region),
         category = as.factor(category),
         sub_category = as.factor(sub_category),
         state = as.factor(state),
         segment = as.factor(segment),
         ship_mode = as.factor(ship_mode),
         order_date = as_date(order_date),
         ship_date = as_date(ship_date))


ui <- page_sidebar(
  
  title = "US SuperStore Online Sales from 2014 - 2018"
  
)

server <- function(input, output, session) {
  
  
  
}

shinyApp(ui = ui, server = server)