
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
vars <- c(
  "Segment" = "segment",
  "State" = "state",
  "Region" = "region",
  "Category" = "category",
  "Sub category" = "sub_category",
  "Ship mode" = "ship_mode"
)

num_vars <- c("Sales" = "sales",
              "Quantity" = "quantity",
              "Discount" = "discount",
              "Profit" = "profit")


ui <- page_sidebar(
  
  title = "US SuperStore Online Sales from 2014 - 2018",
  
  sidebar = sidebar(
    
    #Creating the first categorical variable to subset of
    selectInput(
      inputId = "cat_1",
      label = "Select a categorical variable to subset the data",
      choices = c("None" = "none", 
                  vars),
      selected = "none"
    ),
    
    #Creating the list of elements for the categorical variable they can subset
    
    conditionalPanel(condition = "input.cat_1 != 'none'",
                     uiOutput("cat_values_ui"))
    ,
    
    #This conditionally will show the second option of the categorical variable number 2
    conditionalPanel(condition = "input.cat_1 != 'none'",
      selectInput(
        inputId = "cat_2",
        label = "Select a second categorical variable to subset the data",
        choices = c("None" = "none")
      ),
      
      #Creating a conditional panel for the second categoricla variable subseting
      conditionalPanel(condition = "input.cat_2 != 'none'",
                       uiOutput("cat_values_ui_2"))
  ),
  #Selecting a numeric variable and setting upper and lower bound
  
  selectInput(
    inputId = "num_1",
    label = "Select one numeric variable",
    choices = c("None" = "none",
                num_vars),
    selected = "none"),
  
  #Conditional for slider 1
  conditionalPanel(condition = "input.num_1 != 'none'",
                   uiOutput("num_1_filter_ui")),
  
  #Conditional for second numerical variable
  conditionalPanel(condition = "input.num_1 != 'none'",
    selectInput(
      inputId = "num_2",
      label = "Select a second numeric variable",
      choices = c("None" = "none",
                  num_vars),
      selected = "none"),
    
    #Creating the second conditional slider that will appear when the second numeric variable is selected
    conditionalPanel(condition = "input.num_2 != 'none'",
                     uiOutput("num_2_filter_ui"))
                   
  )
  
  )
  
 )

server <- function(input, output, session) {
  
  #Here we will create the dynamic list for the categorical variables levels, together with a select all and clear all buttons 
  
  output$cat_values_ui <- renderUI({
    
    #Checking that one value is selected
    req(input$cat_1 != "none")
    
    #getting a list of values for the selected categorical variable
    values <- sort(
      unique(as.character(
        store_data[[input$cat_1]]
      )))
    
    #As we will create multiple elements we will use taglist function to have them all
    tagList(
      
      #Creating the list which by default selects all the variables
      selectizeInput(
        inputId = "cat_values",
        label = paste("Select", input$cat_1, "values"),
        choices = values,
        selected = values,
        multiple = TRUE
      ),
      
      #Creating the two buttons
      fluidRow(
        column(6,
               actionButton("select_all_cat", "Select All")),
        
        column(6,
               actionButton("clear_all_cat", "Clear All"))
      )
    )
    
    
  })
  
  #Creating what the select button does
  
  observeEvent(input$select_all_cat, {
    values <- sort(
      unique(as.character(
        store_data[[input$cat_1]]
      )))
    
    #updating the selection
    
    updateSelectizeInput(
      session,
      "cat_values",
      selected = values
    )
    
  })
  
  #Creating what the clear all button does
  
  observeEvent(input$clear_all_cat, {
    updateSelectizeInput(
      session,
      "cat_values",
      selected = character(0)
    )
    
  })
  

  observe({
    
    # Remove the value selected in cat_1
    choices_cat2 <- vars[vars != input$cat_1]
    
    # Add "None" option back
    choices_cat2 <- c("None" = "none", choices_cat2)
    
    updateSelectInput(
      session,
      "cat_2",
      choices = choices_cat2,
      selected = "none"
    )
  })
  
  output$cat_values_ui_2 <- renderUI({
    
    #Checking that one value is selected
    req(input$cat_2 != "none")
    
    #getting a list of values for the selected categorical variable
    values <- sort(
      unique(as.character(
        store_data[[input$cat_2]]
      )))
    
    #As we will create multiple elements we will use taglist function to have them all
    tagList(
      
      #Creating the list which by default selects all the variables
      selectizeInput(
        inputId = "cat_values_2",
        label = paste("Select", input$cat_1, "values"),
        choices = values,
        selected = values,
        multiple = TRUE
      ),
      
      #Creating the two buttons
      fluidRow(
        column(6,
               actionButton("select_all_cat_2", "Select All")),
        
        column(6,
               actionButton("clear_all_cat_2", "Clear All"))
      )
    )
    
    
  })
  
  #Creating what the select button does
  
  observeEvent(input$select_all_cat_2, {
    values <- sort(
      unique(as.character(
        store_data[[input$cat_2]]
      )))
    
    #updating the selection
    
    updateSelectizeInput(
      session,
      "cat_values_2",
      selected = values
    )
    
  })
  
  #Creating what the clear all button does
  
  observeEvent(input$clear_all_cat_2, {
    updateSelectizeInput(
      session,
      "cat_values_2",
      selected = character(0)
    )
    
  })
  
  #Will run this every time one of the variables inside change
  output$num_1_filter_ui <- renderUI({
    
    #The execution of the variable is paused unless a numeric variable is selected
    req(input$num_1 != "none")
    
    #Will get just the column with the selected numeric variable
    num_var <- store_data[[input$num_1]]
    
    #Here we create the slider for the variable number 1
    sliderInput(
      inputId = "num_1_filter",
      label = paste("Filter", input$num_1),
      min = floor(min(num_var, na.rm = TRUE)),
      max = ceiling(max(num_var, na.rm = TRUE)),
      value = c(floor(min(num_var, na.rm = TRUE)),
                ceiling(max(num_var, na.rm = TRUE)))
    )
  })
  
  observe({
    
    # Remove the value selected in num_1
    choices_num2 <- num_vars[num_vars != input$num_1]
    
    # Add "None" option back
    choices_num2 <- c("None" = "none", choices_num2)
    
    updateSelectInput(
      session,
      "num_2",
      choices = choices_num2,
      selected = "none"
    )
  })
  
  #creating the slider for the second numeric variable
  output$num_2_filter_ui <- renderUI({
    
    #The execution of the variable is paused unless a numeric variable is selected
    req(input$num_2 != "none")
    
    #Will get just the column with the selected numeric variable
    num_var <- store_data[[input$num_2]]
    
    #Here we create the slider for the variable number 1
    sliderInput(
      inputId = "num_2_filter",
      label = paste("Filter", input$num_2),
      min = floor(min(num_var, na.rm = TRUE)),
      max = ceiling(max(num_var, na.rm = TRUE)),
      value = c(floor(min(num_var, na.rm = TRUE)),
                ceiling(max(num_var, na.rm = TRUE)))
    )
  })
  
}

shinyApp(ui = ui, server = server)