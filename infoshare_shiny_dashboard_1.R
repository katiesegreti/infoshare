library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(V8)
library(DT)

ui <-  dashboardPage(skin = "blue",
                     dashboardHeader(
                       title="Infoshare"
                     ),
                     dashboardSidebar(
                       width = "300px",
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
                                   selected = "zipcode"),
                       checkboxGroupInput(inputId = "boro", label = "Borough", 
                                          c("Bronx", "Brooklyn", "Manhattan",
                                            "Queens", "Staten Island"), selected = "Bronx",
                                          inline = TRUE),
                       
                       sidebarMenu(
                         # Setting id makes input$tabs give the tabName of currently-selected tab
                         id = "tabs",
                         menuItem("Select demographic data", tabName = "demographic", icon = icon("dashboard"),
                                  menuItem("Population by Sex", 
                                           checkboxGroupInput(inputId = "selected_var_1",
                                                     label = "Select columns:",
                                                     choices = names(nyc_data_2[3:4]))
                                           ),
                                  menuItem("Population by Race", 
                                           checkboxGroupInput(inputId = "selected_var_2",
                                                                                   label = "Select columns:",
                                                                                   choices = names(nyc_data_2[5:12]))
                                  ),
                                  menuItem("Hispanic/Latino population by national origin", 
                                           checkboxGroupInput(inputId = "selected_var_3",
                                                              label = "Select columns:",
                                                              choices = names(nyc_data_2[37:61]))
                                  ), 
                                  menuItem("Asian population by national origin", 
                                           checkboxGroupInput(inputId = "selected_var_4",
                                                              label = "Select columns:",
                                                              choices = names(nyc_data_2[13:36]))
                                  )
                                  ),
                         menuItem("Select socio-economic data", icon = icon("th"), tabName = "widgets",
                                  checkboxGroupInput(inputId = "selected_var_5",
                                                     label = "Select columns:",
                                                     choices = names(nyc_data_2[128:165]))
                                  ),
                         menuItem("Select health data", icon = icon("bar-chart-o"),
                                  menuItem("Male deaths by age",
                                           checkboxGroupInput(inputId = "selected_var_6",
                                                              label = "Select columns:",
                                                              choices = names(nyc_data_2[87:106]))
                                              ),
                                  menuItem("Female deaths by age", tabName = "femaledeaths", 
                                              checkboxGroupInput(inputId = "selected_var_7",
                                                                 label = "Select columns:",
                                                                 choices = names(nyc_data_2[107:126])))
                         )
                       )
                       
                       ),
                     dashboardBody(
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


server <- function(input, output, session) {
  
  
  # Create reactive data frame
  fields_selected <- reactive({
    #req(input$selected_var) # ensure input$selected_var is available
    select(nyc_data_2, c(`Area Name`, input$selected_var_1, input$selected_var_2, input$selected_var_3,
                       input$selected_var_4, input$selected_var_5, input$selected_var_6, input$selected_var_7)) %>%
      filter(nyc_data_2$borough == input$boro | nyc_data_2$borough == "New York City") 
  })
  
  # Create data table
  output$nyc_table <- DT::renderDataTable({
    #req(input$selected_var)
    DT::datatable(data = fields_selected() , 
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

# Run the application 
shinyApp(ui = ui, server = server)
