library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(V8)
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
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(
                      title="Infoshare"
                    ),
    # Input(s)
    dashboardSidebar(
      width = "300px",
      shiny::tags$head(
        shiny::tags$style(HTML("
                        .sidebar { height: 95vh; overflow-y: auto; }
                        " )
        )
        ),
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
      menuItem("Demographic data", tabName = "demographic", 
               menuItem("Population by Sex",
                        radioButtons(inputId = "selected_var1",
                         label = "Select category:",
                         choices = names(data)[14:15],
                         selected = c("Male population"))),
               menuItem("Population by Race",
                        radioButtons(inputId = "selected_var2",
                                     label = "Select category:",
                                     choices = names(data)[16:21])),
               menuItem("Hispanic/Latino population",
                        radioButtons(inputId = "selected_var3",
                                     label = "Select category:",
                                     choices = names(data)[c(22:23, 48:72)])),
               menuItem("Asian population",
                        radioButtons(inputId = "selected_var4",
                                     label = "Select category:",
                                     choices = names(data)[24:47])),
               menuItem("Population by Age",
                        radioButtons(inputId = "selected_var5",
                                     label = "Select category:",
                                     choices = names(data)[74:96]))
               ),
      menuItem("Socio-economic data", tabName = "socioeconomic", 
               radioButtons(inputId = "selected_var6",
                            label = "Select category:",
                            choices = names(data)[139:176])
               ),
      menuItem("Health data", tabName = "health", 
               menuItem("Male Age at Death",
                        radioButtons(inputId = "selected_var7",
                            label = "Select data:",
                            choices = names(data)[98:117])),
               menuItem("Female Age at Death",
                        radioButtons(inputId = "selected_var8",
                                     label = "Select data:",
                                     choices = names(data)[118:137]))
               )
      
    ),
    
    # Output(s)
    dashboardBody(
      h1("Map"),
      leafletOutput("nyc_map",  height = 900)
      
    )
  
)

# Server
server <- function(input, output) {
  
  # Create reactive data frame
  selected_data <- reactive({
    #req(input$selected_var) # ensure input$selected_var is available
    if(input$area_type == "borough") {data1 <- filter(data, borough == input$boro)}
    else{data1 <- data}
    data1 %>% 
      select(c(ZIPCODE, `Area Name`, total_pop,  c(input$selected_var1, input$selected_var2)))
    #select(data, c(ZIPCODE, `Area Name`,  input$selected_var, "total_pop")) # select columns 
  })
  
  # Create data table
  # output$nyc_table <- DT::renderDataTable({
  #   req(input$selected_var)
  #   DT::datatable(data = fields_selected() %>% select(input$selected_var), 
  #                 options = list(pageLength = 25), 
  #                 rownames = FALSE)
  # })
  
  
  # Create map
  output$nyc_map <- renderLeaflet({
    # create color palette with colorNumeric()
    nyc_pal <- colorBin("Blues", reverse = FALSE, bins = 6,
                                   domain = (selected_data()[,4]) / (selected_data()[,3]))
    if(input$area_type == "borough") { map1 <- subset(map, borough == input$boro) }
    else{ map1 <- map }
    map1@data <- selected_data()
    map1 %>%
      leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addPolygons(weight = 1, opacity = 1, fillOpacity = 0.9, color = "white",
                  fillColor = ~nyc_pal((selected_data()[,4]) / (selected_data()[,3])),
                  # add labels that display mean income
                  label = ~paste(`Area Name`, ": ",  round(selected_data()[,4] / (selected_data()[,3]) * 100,2), "%" ),
                  # highlight polygons on hover
                highlight = highlightOptions(weight = 5, color = "white",
                                              bringToFront = TRUE) 
        ) %>% 
      #setView(lng = -73.98, lat = 40.72, zoom = 11) %>%
      addLegend("topleft", pal = nyc_pal, values = ~((selected_data()[,4]) / (selected_data()[,3])),
                title = input$selected_var,
                opacity = 1)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)