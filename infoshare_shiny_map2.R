library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
library(DT)
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
#nyc_data <- read_csv("https://raw.githubusercontent.com/katiesegreti/infoshare/master/nyc_data_sample.csv")
#use local dataset for now
data <- zipz@data
#map <- readOGR("zip_codes/ZIP_CODE_040114.shp")
map <- zipz
# UI
ui <- fluidPage(
  theme = shinytheme("united"),
  # Add the CSS 
  #tags$style(my_css),
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select region  
      selectInput(inputId = "region", 
                  label = "Region:",
                  choices = c("New York City" = "nyc", 
                              "New York State" = "nys", 
                              "USA" = "usa"), 
                  selected = "nyc"),
      # Select overal area type  
      selectInput(inputId = "area_type", 
                  label = "Overall Area Type:",
                  choices = c("City" = "city", 
                              "Borough" = "borough"), 
                  selected = "city"),
      # Conditional panel for borough selection
      conditionalPanel(condition = "input.area_type == 'borough'",
                       selectInput(inputId = "boro",
                                   label = "Select Borough:",
                                   choices = c("Bronx", "Brooklyn", "Manhattan", "Queens",
                                               "Staten Island"),
                                   selected = "Bronx")
        
      ),
      # Select area to compare  
      selectInput(inputId = "area_compare", 
                  label = "Areas to Compare:",
                  choices = c("Borough" = "borough", 
                              "Zip Code" = "zipcode",
                              "Census Tract" = "censustract",
                              "Community District" = "communitydistrtict"), 
                  selected = "zipcode"),
      # Select variables to view
      radioButtons(inputId = "selected_var",
                         label = "Select data:",
                         choices = names(data)[14:123],
                         selected = c("Male population"))
      
    ),
    
    # Output(s)
    mainPanel(
     tabsetPanel( 
       tabPanel(
        title = "Map",
      leafletOutput("nyc_map",  height = 900)),
      tabPanel(
        title = "Table",
        DT::dataTableOutput(outputId = "table1")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  selected_data <- reactive({
    req(input$selected_var) # ensure input$selected_var is available
    if(input$area_type == "borough") {data1 <- filter(data, borough == input$boro)}
    else{data1 <- data}
    selected <- input$selected_var
    data1 %>% 
      select(c(ZIPCODE, `Area Name`,  input$selected_var, total_pop)) %>%
      mutate(pct = round(data1[,selected] / total_pop, 2) * 100) %>%
      filter(!is.na(pct))
  })
  
  # Create data table
  output$table1 <- DT::renderDataTable({
  #   req(input$selected_var)
    DT::datatable(data = selected_data(), 
                   options = list(pageLength = 25), 
                   rownames = FALSE) %>%
      DT::formatStyle(names(selected_data()),
                      background = DT::styleColorBar(range(selected_data()[5]), 'lightblue'),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
    
   })
  
  
  # Create map
  output$nyc_map <- renderLeaflet({
    # create color palette with colorNumeric()
    nyc_pal <- colorBin("Blues", reverse = FALSE, bins = 6,
                                   domain = (selected_data()[,3]) / (selected_data()[,4]))
    if(input$area_type == "borough") { map1 <- subset(map, borough == input$boro) }
    else{ map1 <- map }
    map1@data <- selected_data()
    map1 %>%
      leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(weight = 1, opacity = 1, fillOpacity = 0.9, color = "white",
                  fillColor = ~nyc_pal((selected_data()[,3]) / (selected_data()[,4])),
                  # add labels that display mean income
                  label = ~paste(`Area Name`, ": ",  round(selected_data()[,3] / (selected_data()[,4]) * 100,2), "%" ),
                  # highlight polygons on hover
                highlight = highlightOptions(weight = 5, color = "white",
                                              bringToFront = TRUE) 
        ) %>% 
      #setView(lng = -73.98, lat = 40.72, zoom = 11) %>%
      addLegend("topleft", pal = nyc_pal, values = ~((selected_data()[,3]) / (selected_data()[,4])),
                title = input$selected_var,
                opacity = 1)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)