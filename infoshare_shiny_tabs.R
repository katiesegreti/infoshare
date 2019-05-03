library(shiny)
library(dplyr)
library(readr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  h1("Area Comparison"),
  # Create a container for tab panels
  tabsetPanel(
    # Create an "Inputs" tab
    tabPanel(
      title = "Select Data",
      # Select region  
      selectInput(inputId = "region", 
                  label = "Region:",
                  choices = c("New York City" = "nyc", 
                              "New York State" = "nys", 
                              "USA" = "usa"), 
                  selected = "New York City"),
      # Select overal area type  
      selectInput(inputId = "area_type", 
                  label = "Overall Area Type:",
                  choices = c("City" = "city", 
                              "Borough" = "borough"), 
                  selected = "City"),
      # Select area to compare  
      selectInput(inputId = "area_type", 
                  label = "Areas to Compare:",
                  choices = c("Borough" = "borough", 
                              "Zip Code" = "zipcode",
                              "Census Tract" = "censustract",
                              "Community District" = "communitydistrtict"), 
                  selected = "City"),
      # Select variables to view
      fluidRow(
        column(4, 
               checkboxGroupInput(inputId = "selected_var",
                         label = "Select demographic data:",
                         choices = names(nyc_data[2:32]),
                         selected = c("Area Name"))),
        column(4,
               checkboxGroupInput(inputId = "selected_var_2",
                         label = "Select health data:",
                         choices = names(nyc_data[34:73]))),
        column(4,
               checkboxGroupInput(inputId = "selected_var_3",
                                  label = "Select socio-economic data:",
                                  choices = names(nyc_data[75:112]),
                                  selected = c("Area Name")))
      )
    ),
    # Create a "Plot" tab
    # tabPanel(
    #   title = "Plot",
    #   plotOutput("plot")
    # ),
    # Create "Table" tab
    tabPanel(
      title = "Table",
      DT::dataTableOutput(outputId = "nyc_table")
    )
  )
)

server <- function(input, output) {
  # filtered_data <- reactive({
  #   data <- gapminder
  #   data <- subset(
  #     data,
  #     lifeExp >= input$life[1] & lifeExp <= input$life[2]
  #   )
  #   if (input$continent != "All") {
  #     data <- subset(
  #       data,
  #       continent == input$continent
  #     )
  #   }
  #   data
  # })
  # Create reactive data frame
  fields_selected <- reactive({
    req(input$selected_var) # ensure input$selected_var is available
    select(nyc_data, c(input$selected_var, input$selected_var_2,
                       input$selected_var_3)) # select columns 
  })
  
  
  # Create data table
  output$nyc_table <- DT::renderDataTable({
    req(input$selected_var)
    DT::datatable(data = fields_selected() %>% select(c(input$selected_var, 
                                                        input$selected_var_2,
                                                        input$selected_var_3)), 
                  options = list(pageLength = 25), 
                  rownames = FALSE)
  })
  
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("infoshare.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(fields_selected() %>% select(input$selected_var), file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(fields_selected() %>% select(input$selected_var), file) 
      }
    }
  )
  
#   output$plot <- renderPlot({
#     data <- filtered_data()
#     ggplot(data, aes(gdpPercap, lifeExp)) +
#       geom_point() +
#       scale_x_log10()
#   })
}

shinyApp(ui, server)