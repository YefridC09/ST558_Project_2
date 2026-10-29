# ST558_Project_2
# US SuperStore Online Sales Explorer

A Shiny app for exploring online sales data from a fictional US retail company, the SuperStore. Users can subset the data, view and download it, and create numerical and graphical summaries to see which products, segments and regions bring the most sales and profit.

**Live app:** <https://yacordob.shinyapps.io/store_app/>

## Data

The data come from the [Sample Superstore data set](https://www.kaggle.com/datasets/juhi1994/superstore/data). Each row is an order for each product sold, with categorical variables (segment, region, state, category, sub category, ship mode) and numeric variables (sales, quantity, discount, profit), plus order and ship dates and ZIP codes.

## What the app does

- **Sidebar:** subset the data by up to two categorical variables (with Select All / Clear All) and up to two numeric variables (with range sliders). The data only updates when the **Get data** button is clicked.
- **About:** describes the app, the data and how to use each part of the app.
- **Data Download:** shows the (subsetted) data in a table and saves it as a csv file.
- **Data Exploration:**
  - *Categorical summaries:* one-way and two-way contingency tables and bar charts (stacked, side by side or proportions).
  - *Numeric summaries:* count, mean, median, standard deviation, minimum and maximum, overall or by a categorical variable.
  - *Graphs:* boxplots, violin plots, histograms, scatterplots and a map of sales by ZIP code, with optional color and facet variables.

## Files

- `app.R`: the Shiny app.
- `static_project_2.R`: the static exploration of the data used to design the app.
- `assets/`: the data file and the image used in the app.

## Running the app locally

Install the required packages and run the app from RStudio:

```r
install.packages(c("shiny", "bslib", "tidyverse", "readxl", "lubridate",
                   "janitor", "zipcodeR", "maps", "DT", "shinycssloaders"))

shiny::runApp()
```
