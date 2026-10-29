
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(zipcodeR)
library(maps)
library(DT)

#First we are going to import the data, which is going to be run one time when the app is launched
store_data <- read_excel("assets/superstore_data.xls") |>
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

#cahngong the browser path
addResourcePath(
  prefix = "assets",
  directoryPath = "assets"
)

#Function that returns the name shown to the user (i.e. "Sub category") from the name of the column (i.e. "sub_category")
get_label <- function(value, lookup) {
  names(lookup)[match(value, lookup)]
}

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
                   
  ),
  
  #Creating the button to get the data.
  actionButton("get_data", "Get data")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "About",
        "This app is used to explore the data from online sales of the SuperStore, ...",
        tags$figure(
          class = "centerFigure",
          tags$img(
            src = "assets/superstore.png",
            width = "100%",
            style = "max-width: 600px; height: auto;",
            alt = "SuperStore sales dashboard image"
          ),
          tags$figcaption(
            "US SuperStore online sales"
          )
        )
      ),
      tabPanel(
        "Data Download",
        DT::dataTableOutput("table_filtered"),
        downloadButton("download_data", "Download")
      ),
      #Creating the data exploration tab
      tabPanel(
        "Data Exploration",
        
        #Adding a title and instructions for the data exploration section
        h2("Explore the filtered SuperStore data"),
        
        p(
          "The summaries and graphs below use the data obtained when the ",
          strong("Get data"),
          " button is clicked."
        ),
        
        #Creating subtabs to separate the different types of summaries
        tabsetPanel(
          

          tabPanel(
            "Categorical summaries",
            
            br(),
            
            #Creating the inputs used to build the categorical summaries
            fluidRow(
              
              #Selecting the first categorical variable
              column(
                width = 4,
                
                selectInput(
                  inputId = "summary_cat_1",
                  label = "Select the first categorical variable",
                  choices = vars,
                  selected = "segment"
                )
              ),
              
              #Selecting an optional second categorical variable
              column(
                width = 4,
                
                selectInput(
                  inputId = "summary_cat_2",
                  label = "Select an optional second categorical variable",
                  choices = c(
                    "None" = "none",
                    vars
                  ),
                  selected = "none"
                )
              ),
              
              #Selecting how the bars will be displayed when two variables are selected
              column(
                width = 4,
                
                selectInput(
                  inputId = "bar_position",
                  label = "Select the bar chart position",
                  choices = c(
                    "Stacked" = "stack",
                    "Side by side" = "dodge",
                    "Proportions" = "fill"
                  ),
                  selected = "stack"
                )
              )
            ),
            
            #Creating the location where the categorical table will be displayed
            h4("Contingency table"),
            
            DT::dataTableOutput(
              outputId = "categorical_summary_table"
            ),
            
            br(),
            
            #Creating the location where the categorical graph will be displayed
            h4("Categorical bar chart"),
            
            plotOutput(
              outputId = "categorical_summary_plot",
              height = "500px"
            )
          ),
          
          
          tabPanel(
            "Numeric summaries",
            
            br(),
            
            #Creating the inputs used to generate the numeric summaries
            fluidRow(
              
              #Selecting the numeric variable that will be summarized
              column(
                width = 6,
                
                selectInput(
                  inputId = "summary_num",
                  label = "Select a numeric variable to summarize",
                  choices = num_vars,
                  selected = "sales"
                )
              ),
              
              #Selecting an optional categorical variable to group the observations
              column(
                width = 6,
                
                selectInput(
                  inputId = "summary_group",
                  label = "Optionally summarize across a categorical variable",
                  choices = c(
                    "No grouping" = "none",
                    vars
                  ),
                  selected = "none"
                )
              )
            ),
            
            #Creating the location where the numeric table will be displayed
            h4("Numeric summary statistics"),
            
            DT::dataTableOutput(
              outputId = "numeric_summary_table"
            )
          ),
          
       
          tabPanel(
            "Graphs",
            
            br(),
            
            #Creating the first row of graph options
            fluidRow(
              
              #Selecting the type of graph that will be created
              column(
                width = 4,
                
                selectInput(
                  inputId = "explore_plot_type",
                  label = "Select the type of graph",
                  choices = c(
                    "Boxplot" = "boxplot",
                    "Violin plot" = "violin",
                    "Histogram" = "histogram",
                    "Scatterplot" = "scatterplot"
                  ),
                  selected = "boxplot"
                )
              ),
              
              #Selecting the numeric variable that will be plotted
              column(
                width = 4,
                
                selectInput(
                  inputId = "explore_num_y",
                  label = "Select the numeric variable",
                  choices = num_vars,
                  selected = "sales"
                )
              ),
              
              #Displaying a second numeric variable only for scatterplots
              column(
                width = 4,
                
                conditionalPanel(
                  condition = "input.explore_plot_type == 'scatterplot'",
                  
                  selectInput(
                    inputId = "explore_num_x",
                    label = "Select the numeric x variable",
                    choices = num_vars,
                    selected = "profit"
                  )
                )
              )
            ),
            
            #Creating the second row of graph options
            fluidRow(
              
              #Selecting the categorical variable for the x axis
              column(
                width = 4,
                
                selectInput(
                  inputId = "explore_cat_x",
                  label = "Select a categorical x variable",
                  choices = vars,
                  selected = "category"
                )
              ),
              
              #Selecting an optional categorical variable for color
              column(
                width = 4,
                
                selectInput(
                  inputId = "explore_color",
                  label = "Optionally color by a categorical variable",
                  choices = c(
                    "No color variable" = "none",
                    vars
                  ),
                  selected = "none"
                )
              ),
              
              #Selecting an optional categorical variable to facet the graph
              column(
                width = 4,
                
                selectInput(
                  inputId = "explore_facet",
                  label = "Optionally facet by a categorical variable",
                  choices = c(
                    "No facet variable" = "none",
                    vars
                  ),
                  selected = "none"
                )
              )
            ),
            
            #Displaying the number of bins option only for histograms
            conditionalPanel(
              condition = "input.explore_plot_type == 'histogram'",
              
              sliderInput(
                inputId = "histogram_bins",
                label = "Number of histogram bins",
                min = 5,
                max = 100,
                value = 30,
                step = 5
              )
            ),
            
            #Creating the location where the selected graph will be displayed
            plotOutput(
              outputId = "exploration_plot",
              height = "600px"
            )
          )
        )
      )
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
        label = paste("Select", input$cat_2, "values"),
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
  
  #Creating the reactive context for the button so it actually gets the data
  filtered_data <- eventReactive(input$get_data,{
    
    data <- store_data
    
    #Applying the filter to the first numeric variable
    if(!is.null(input$num_1) && input$num_1 != "none"){
      
      rng <- input$num_1_filter
      
      data <- data |>
        filter(.data[[input$num_1]] >= rng[1],
               .data[[input$num_1]] <= rng[2])
    }
    
    #Applying numeric filter 2
    if(!is.null(input$num_2) && input$num_2 != "none"){
      
      rng <- input$num_2_filter
      
      data <- data |>
        filter(.data[[input$num_2]] >= rng[1],
               .data[[input$num_2]] <= rng[2])
    }
    
    #Applying filter from first categorical variable
    if(!is.null(input$cat_1) && input$cat_1 != "none"){
      
      
      data <- data |>
        filter(.data[[input$cat_1]] %in% input$cat_values)
    }
    
    #Applying filter number 2 for categorical variable
    if(!is.null(input$cat_2) && input$cat_2 != "none"){
      
      
      data <- data |>
        filter(.data[[input$cat_2]] %in% input$cat_values_2)
    }
    
#    print("event running")
    data
    
  },

#Returning the unfiltered data before the button is clicked for the first time
ignoreNULL = FALSE
)
  
  
  #Creating either a one-way or two-way contingency table
  output$categorical_summary_table <- DT::renderDataTable({
    
    #Getting the most recent data created by the Get data button
    data <- filtered_data()
    
    #Displaying a message instead of an error when no observations are returned
    validate(
      need(
        nrow(data) > 0,
        "The selected filters returned no observations."
      )
    )
    
    #Pausing the execution until the first categorical variable is available
    req(input$summary_cat_1)
    
    #Creating a one-way contingency table when no second variable is selected
    if (
      is.null(input$summary_cat_2) ||
      input$summary_cat_2 == "none"
    ) {
      
      #Creating a one-way contingency table using the selected variable
      summary_table <- table(
        data[[input$summary_cat_1]],
        useNA = "ifany"
      )
      
      #Converting the table into a data frame so DT can display it
      summary_table <- as.data.frame(
        summary_table
      )
      
      #Changing the automatically created column names
      names(summary_table) <- c(
        input$summary_cat_1,
        "count"
      )
      
      #Arranging the table from the highest to the lowest count
      summary_table <- summary_table[
        order(
          summary_table$count,
          decreasing = TRUE
        ),
      ]
      
    } else {
      
      #Creating a two-way contingency table using the selected variables
      summary_table <- table(
        data[[input$summary_cat_1]],
        data[[input$summary_cat_2]],
        useNA = "ifany"
      )
      
      #Converting the table into a data frame so DT can display it
      summary_table <- as.data.frame.matrix(
        summary_table
      )
      
      #Creating a column containing the levels of the first variable
      summary_table <- cbind(
        category = row.names(summary_table),
        summary_table
      )
      
      #Changing the name of the first column to the selected variable
      names(summary_table)[1] <- input$summary_cat_1
    }
    
    #Removing the row names created by the table conversion
    row.names(summary_table) <- NULL
    
    #Returning the categorical summary table
    summary_table
    
  }
  )
  
  #Displaying the data obtained when the Get data button is clicked
  output$table_filtered <- DT::renderDataTable({
    
    #Getting the most recent data created by the Get data button
    filtered_data()
    
  })
  
  #Creating the file that will be downloaded when the Download button is clicked
  output$download_data <- downloadHandler(
    
    #Name of the file, it includes the date to avoid overwriting older downloads
    filename = function() {
      paste0("superstore_data_", Sys.Date(), ".csv")
    },
    
    #Saving the data as a csv file
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  
  #Creating a bar chart with one or two categorical variables
  output$categorical_summary_plot <- renderPlot({
    
    #Getting the most recent data created by the Get data button
    data <- filtered_data()
    
    #Displaying a message instead of an error when no observations are returned
    validate(
      need(
        nrow(data) > 0,
        "The selected filters returned no observations."
      )
    )
    
    #Pausing the execution until the inputs are available
    req(input$summary_cat_1, input$bar_position)
    
    #Saving the inputs in objects, so they are easier to use inside the plot
    cat_1 <- input$summary_cat_1
    cat_2 <- input$summary_cat_2
    x_label <- get_label(cat_1, vars)
    
    #Creating a simple bar chart when no second variable is selected
    if (is.null(cat_2) || cat_2 == "none") {
      
      p <- ggplot(data = data,
                  aes(x = .data[[cat_1]])) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Number of sales by", x_label),
             x = x_label,
             y = "Count")
      
    } else {
      
      #The two variables have to be different to create a meaningful graph
      validate(
        need(
          cat_1 != cat_2,
          "Please select two different categorical variables."
        )
      )
      
      #The label of the y axis changes when the bars are displayed as proportions
      fill_label <- get_label(cat_2, vars)
      y_label <- ifelse(input$bar_position == "fill", "Proportion", "Count")
      
      #Creating the bar chart colored by the second variable, the position comes from the user
      p <- ggplot(data = data,
                  aes(x = .data[[cat_1]],
                      fill = .data[[cat_2]])) +
        geom_bar(position = input$bar_position) +
        labs(title = paste("Number of sales by", x_label, "and", fill_label),
             x = x_label,
             y = y_label,
             fill = fill_label)
    }
    
    #Adding the theme, the labels are rotated because some variables have many levels
    p +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
  })
}

shinyApp(ui = ui, server = server)