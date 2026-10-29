
library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(zipcodeR)
library(maps)
library(DT)
library(shinycssloaders)

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

#Getting the polygons of the US states, they are used as the background of the map
us_map <- map_data("state")

#Getting the coordinates of each ZIP code, they are matched with the sales when the map is created
zip_lookup <- zip_code_db |>
  dplyr::select(zipcode, lat, lng)

ui <- page_sidebar(
  
  title = h1(strong("US SuperStore Online Sales from 2014 - 2018")),
  
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
        
        br(),
        
        #Describing the purpose of the app
        h2("Purpose of the app"),
        
        p(
          "This app allows you to explore the online sales of the US SuperStore. ",
          "You can choose a part of the data (for example, one region or the sales with a discount),",
          " look at it as a table, download it, and create numerical and graphical summaries to find ",
          "which products, customers and regions bring the most sales and profit."
        ),
        
        #Describing the data and giving the link to its source
        h2("The data"),
        
        p(
          "The data are the orders of the ",
          a(
            "Sample Superstore data set",
            href = "https://www.kaggle.com/datasets/juhi1994/superstore/data",
            target = "_blank"
          ),
          ", a fictional US retail company used to practice data analysis. ",
          "Each row is one product sold in an order. The data has ",
          strong(format(nrow(store_data), big.mark = ",")),
          " rows with orders placed from ",
          strong(format(min(store_data$order_date, na.rm = TRUE), "%B %Y")),
          " to ",
          strong(format(max(store_data$order_date, na.rm = TRUE), "%B %Y")),
          ". The variables used in the app are:"
        ),
        
        #Listing the variables of the data
        tags$ul(
          tags$li(
            strong("Categorical variables: "),
            "segment (consumer, corporate or home office), region, state, ",
            "category, sub category and ship mode."
          ),
          tags$li(
            strong("Numeric variables: "),
            "sales (in dollars), quantity, discount (from 0 to 1) and profit (in dollars, ",
            "it is negative when the store loses money)."
          ),
          tags$li(
            strong("Other variables: "),
            "order date, ship date and ZIP code (used to build the map)."
          )
        ),
        
        #Describing the purpose of the sidebar
        h2("How to use the sidebar"),
        
        p(
          "The sidebar is used to ",
          strong("subset the data"),
          " before it is displayed, summarized or plotted:"
        ),
        
        tags$ul(
          tags$li(
            "Select a categorical variable and choose which of its levels you want to keep. ",
            "All levels are selected at the beginning, and the ",
            em("Select All"),
            " and ",
            em("Clear All"),
            " buttons help you to change the selection quickly. Once the first variable is chosen, ",
            "you can select a second categorical variable and filter it in the same way."
          ),
          tags$li(
            "Select a numeric variable and use the slider to choose the lower and upper bound ",
            "of the values you want to keep. Once the first variable is chosen, ",
            "you can select a second numeric variable with its own slider."
          ),
          tags$li(
            "Click the ",
            strong("Get data"),
            " button to apply the filters. ",
            strong("The data does not change until this button is clicked"),
            ", so you can change the widgets as many times as you want before applying them."
          )
        ),
        
        #Describing the purpose of each tab
        h2("What you can find in each tab"),
        
        tags$ul(
          tags$li(
            strong("About: "),
            "the page you are reading, with the description of the app and the data."
          ),
          tags$li(
            strong("Data Download: "),
            "a table with the data obtained with the sidebar. The ",
            em("Download"),
            " button saves this data as a csv file."
          ),
          tags$li(
            strong("Data Exploration: "),
            "summaries and graphs of the data obtained with the sidebar. It has three sub tabs:",
            tags$ul(
              tags$li(
                strong("Categorical summaries: "),
                "a one-way or two-way contingency table and a bar chart (stacked, side by side ",
                "or as proportions) for the categorical variables you choose."
              ),
              tags$li(
                strong("Numeric summaries: "),
                "the count, mean, median, standard deviation, minimum and maximum of a numeric ",
                "variable, for all the data or at the levels of a categorical variable."
              ),
              tags$li(
                strong("Graphs: "),
                "boxplots, violin plots, histograms, scatterplots and a map of the sales by ZIP code. ",
                "You can choose the variables and color or facet the graph by a categorical variable. ",
                "Sales are shown in a logarithmic scale in the boxplots, violin plots and histograms ",
                "because a few very large sales would hide the rest of the values."
              )
            )
          )
        ),
        
        br(),
        
        #Adding a picture related to the data
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
        withSpinner(
        DT::dataTableOutput("table_filtered")),
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
            
            withSpinner(
            DT::dataTableOutput(
              outputId = "categorical_summary_table"
            )),
            
            br(),
            
            #Creating the location where the categorical graph will be displayed
            h4("Categorical bar chart"),
            
            withSpinner(
            plotOutput(
              outputId = "categorical_summary_plot",
              height = "500px"
            ))
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
            
            withSpinner(
            DT::dataTableOutput(
              outputId = "numeric_summary_table"
            ))
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
            withSpinner(
            plotOutput(
              outputId = "exploration_plot",
              height = "600px"
            ))
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
  
  #Creating the numeric summaries, for all the data or at the levels of a categorical variable
  output$numeric_summary_table <- DT::renderDataTable({
    
    #Getting the most recent data created by the Get data button
    data <- filtered_data()
    
    #Displaying a message instead of an error when no observations are returned
    validate(
      need(
        nrow(data) > 0,
        "The selected filters returned no observations."
      )
    )
    
    #Pausing the execution until the numeric variable is available
    req(input$summary_num)
    
    #Saving the inputs in objects, so they are easier to use inside the summary
    num_var <- input$summary_num
    group_var <- input$summary_group
    
    #Grouping the data only when the user selects a categorical variable
    if (is.null(group_var) || group_var == "none") {
      grouped_data <- data
    } else {
      grouped_data <- data |>
        group_by(across(all_of(group_var)))
    }
    
    #Calculating the summaries and sorting them from the highest to the lowest count
    summary_table <- grouped_data |>
      summarize(n = n(),
                mean = round(mean(.data[[num_var]], na.rm = TRUE), 2),
                median = round(median(.data[[num_var]], na.rm = TRUE), 2),
                sd = round(sd(.data[[num_var]], na.rm = TRUE), 2),
                min = round(min(.data[[num_var]], na.rm = TRUE), 2),
                max = round(max(.data[[num_var]], na.rm = TRUE), 2),
                .groups = "drop") |>
      arrange(desc(n))
    
    #Returning the numeric summary table
    summary_table
    
  })
  
  output$exploration_plot <- renderPlot({
    
    #Getting the most recent data created by the Get data button
    data <- filtered_data()
    
    #Displaying a message instead of an error when no observations are returned
    validate(
      need(
        nrow(data) > 0,
        "The selected filters returned no observations."
      )
    )
    
    #Pausing the execution until the main inputs are available
    req(input$explore_plot_type, input$explore_num_y, input$explore_cat_x)
    
    #Saving the inputs in objects, so they are easier to use inside the plot
    plot_type <- input$explore_plot_type
    num_y <- input$explore_num_y
    cat_x <- input$explore_cat_x
    color_var <- input$explore_color
    facet_var <- input$explore_facet
    
    #Checking if the user selected the optional color and facet variables
    has_color <- !is.null(color_var) && color_var != "none"
    has_facet <- !is.null(facet_var) && facet_var != "none"
    
    #Getting the names that will be displayed in the labels of the graph
    y_label <- get_label(num_y, num_vars)
    x_label <- get_label(cat_x, vars)
    color_label <- if (has_color) get_label(color_var, vars) else NULL
    
    #Sales is very skewed (a few very big sales), so a logarithmic scale is used for it as in the static file
    use_log <- num_y == "sales"
    
    #--------------------------------------------------------
    #Boxplot and violin plot
    #--------------------------------------------------------
    if (plot_type %in% c("boxplot", "violin")) {
      
      #The categorical variable goes in the x axis and the numeric variable in the y axis
      p <- ggplot(data = data,
                  aes(x = .data[[cat_x]],
                      y = .data[[num_y]]))
      
      #Creating the geom with or without the color variable
      if (has_color) {
        
        p <- p + aes(fill = .data[[color_var]])
        
        geom_layer <- if (plot_type == "boxplot") geom_boxplot() else geom_violin()
        
      } else {
        
        geom_layer <- if (plot_type == "boxplot") {
          geom_boxplot(fill = "steelblue")
        } else {
          geom_violin(fill = "steelblue")
        }
      }
      
      plot_name <- ifelse(plot_type == "boxplot", "Boxplot", "Violin plot")
      
      p <- p +
        geom_layer +
        labs(title = paste(plot_name, "of", y_label, "by", x_label),
             x = x_label,
             y = ifelse(use_log, paste(y_label, "(log scale)"), y_label),
             fill = color_label) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      #Applying the logarithmic scale only to sales
      if (use_log) {
        p <- p + scale_y_log10()
      }
      
      #--------------------------------------------------------
      #Histogram
      #--------------------------------------------------------
    } else if (plot_type == "histogram") {
      
      req(input$histogram_bins)
      
      p <- ggplot(data = data,
                  aes(x = .data[[num_y]]))
      
      #Creating the histogram with or without the color variable
      if (has_color) {
        
        p <- p +
          aes(fill = .data[[color_var]]) +
          geom_histogram(bins = input$histogram_bins)
        
      } else {
        
        p <- p +
          geom_histogram(bins = input$histogram_bins,
                         fill = "steelblue",
                         color = "white")
      }
      
      p <- p +
        labs(title = paste("Histogram of", y_label),
             x = ifelse(use_log, paste(y_label, "(log scale)"), y_label),
             y = "Count",
             fill = color_label) +
        theme_bw()
      
      #Applying the logarithmic scale only to sales
      if (use_log) {
        p <- p + scale_x_log10()
      }
      
      #--------------------------------------------------------
      #Scatterplot
      #--------------------------------------------------------
    } else if (plot_type == "scatterplot") {
      
      #Pausing the execution until the x variable is available
      req(input$explore_num_x)
      
      num_x <- input$explore_num_x
      x_num_label <- get_label(num_x, num_vars)
      
      #The two numeric variables have to be different to create a meaningful graph
      validate(
        need(
          num_x != num_y,
          "Please select two different numeric variables for the scatterplot."
        )
      )
      
      p <- ggplot(data = data,
                  aes(x = .data[[num_x]],
                      y = .data[[num_y]]))
      
      #Creating the points with or without the color variable
      if (has_color) {
        
        p <- p +
          aes(colour = .data[[color_var]]) +
          geom_point(alpha = 0.6)
        
      } else {
        
        p <- p +
          geom_point(alpha = 0.6,
                     colour = "steelblue")
      }
      
      p <- p +
        labs(title = paste(y_label, "vs", x_num_label),
             x = x_num_label,
             y = y_label,
             colour = color_label) +
        theme_bw()
      
      #--------------------------------------------------------
      #Map by ZIP code
      #--------------------------------------------------------
    } else {
      
      #Adding the coordinates of each ZIP code, the zeros lost by excel are added back (i.e. 5408 is 05408)
      map_points <- data |>
        mutate(postal_code = str_pad(as.character(postal_code),
                                     width = 5,
                                     side = "left",
                                     pad = "0")) |>
        left_join(zip_lookup,
                  by = c("postal_code" = "zipcode")) |>
        drop_na(lat, lng)
      
      #Displaying a message instead of an error when no ZIP code is found
      validate(
        need(
          nrow(map_points) > 0,
          "No ZIP code coordinates were found for the selected data."
        )
      )
      
      #The polygons are the background and the points are the sales, the size and color use the numeric variable
      p <- ggplot() +
        geom_polygon(data = us_map,
                     aes(x = long, y = lat, group = group),
                     fill = "gray",
                     color = "white",
                     linewidth = 0.2) +
        geom_point(data = map_points,
                   aes(x = lng,
                       y = lat,
                       size = .data[[num_y]],
                       color = .data[[num_y]]),
                   alpha = 0.7) +
        scale_color_viridis_c() +
        coord_fixed(1.3) +
        labs(title = paste(y_label, "by ZIP code"),
             color = y_label,
             size = y_label) +
        theme_void()
    }
    
    #Adding the facets when the user selects a variable (the map background is repeated in every panel)
    if (has_facet) {
      p <- p + facet_wrap(facet_var)
    }
    
    #Returning the graph
    p
    
  })
  
}

shinyApp(ui = ui, server = server)