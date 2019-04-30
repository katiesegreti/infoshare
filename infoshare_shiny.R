library(shiny)
library(dplyr)
library(readr)
#load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),
      
      # Select variables to download
      checkboxGroupInput(inputId = "selected_var",
                         label = "Select variables:",
                         choices = names(nyc_data),
                         selected = c("Area Name"))
      
    ),
    
    # Output(s)
    mainPanel(
      HTML("Select filetype and variables, then download and/or view the data."),
      br(), br(),
      downloadButton(outputId = "download_data", label = "Download data"),
      br(), br(),
      DT::dataTableOutput(outputId = "nyc_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  fields_selected <- reactive({
    req(input$selected_var) # ensure input$selected_var is available
    select(nyc_data, input$selected_var) # select columns of movies
  })
  
  # Create data table
  output$nyc_table <- DT::renderDataTable({
    req(input$selected_var)
    DT::datatable(data = fields_selected() %>% select(input$selected_var), 
                  options = list(pageLength = 10), 
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
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)