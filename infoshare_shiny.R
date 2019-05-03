library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
# css
my_css <- "
#download_data {
  /* Change the background colour of the download button
     to orange. */
  background: orange;

  /* Change the text size to 20 pixels. */
  
}

#table {
  /* Change the text colour of the table to red. */
  
}
"

# UI
ui <- fluidPage(
  theme = shinytheme("united"),
  # Add the CSS 
  tags$style(my_css),
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
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
      checkboxGroupInput(inputId = "selected_var",
                         label = "Select data:",
                         choices = names(nyc_data),
                         selected = c("Area Name"))
      
    ),
    
    # Output(s)
    mainPanel(
      h1("Area Comparison"),
      #br(),
      # Select filetype
      # radioButtons(inputId = "filetype",
      #              label = "Select filetype:",
      #              choices = c("csv", "tsv"),
      #              selected = "csv"),
      
      #br(),
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
    select(nyc_data, input$selected_var) # select columns 
  })
  
  # Create data table
  output$nyc_table <- DT::renderDataTable({
    req(input$selected_var)
    DT::datatable(data = fields_selected() %>% select(input$selected_var), 
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
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
