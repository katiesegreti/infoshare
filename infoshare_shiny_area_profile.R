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
                  label = "Area Type:",
                  choices = c("Zip Code" = "zipcode", 
                              "Borough" = "borough",
                              "Census Tract" = "censustract"), 
                  selected = "Zip Code"),
      # Select area to compare  
      selectInput(inputId = "zip", 
                  label = "Zip Code:",
                  choices = c("11209 - Bay Ridge" = "bayridge", 
                              "11211 - Williamsburg" = "williamsburg",
                              "11217 - Park Slope/Gowanus" = "gowanus"), 
                  selected = "11209 - Bay Ridge"),
      # Select variables to view
      checkboxGroupInput(inputId = "selected_var",
                         label = "Select data:",
                         choices = names(nyc_data),
                         selected = c("Area Name"))
      
    ),
    
    # Output(s)
    mainPanel(
      h1("Area Profile"),
      #br(),
      # Select filetype
      # radioButtons(inputId = "filetype",
      #              label = "Select filetype:",
      #              choices = c("csv", "tsv"),
      #              selected = "csv"),
      
      #br(),
      downloadButton(outputId = "download_data", label = "Download data"),
      br(), br(),
      #DT::dataTableOutput(outputId = "nyc_table")
      DT::dataTableOutput(outputId = "table_3"),
      br(), br(),
      DT::dataTableOutput(outputId = "table_4"),
      br(), br(),
      DT::dataTableOutput(outputId = "table_1"),
      br(), br(),
      DT::dataTableOutput(outputId = "table_2")
    )
  )
)

# Server
server <- function(input, output) {
  
  male_table <- male_deaths_11209
  female_table <- female_deaths_11209
  pop_table <- pop_by_sex
  pop_race_table <- pop_by_race
  hh_table <- pop_by_hh_size

  #first data table
  output$table_3 <- DT::renderDataTable({
    DT::datatable(data = pop_race_table, options = list(pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatStyle(names(pop_race_table),
                      background = DT::styleColorBar(range(pop_race_table[2]), 'lightblue'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
    
  })
  #first data table
  output$table_4 <- DT::renderDataTable({
    DT::datatable(data = pop_by_hh_size, options = list(pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatStyle(names(pop_by_hh_size),
                      background = DT::styleColorBar(range(pop_by_hh_size[2]), 'lightblue'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
    
  })
  #first data table
  output$table_1 <- DT::renderDataTable({
    DT::datatable(data = male_table, options = list(pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatStyle(names(male_table),
                   background = DT::styleColorBar(range(male_table[2]), 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
  })
  #second data table
  output$table_2 <- DT::renderDataTable({
    DT::datatable(data = female_table, options = list(pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatStyle(names(female_table),
                      background = DT::styleColorBar(range(female_table[2]), 'lightblue'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
    
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
