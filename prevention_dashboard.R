library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)


prev <- read_csv("https://raw.githubusercontent.com/katiesegreti/infoshare/master/prevention_clean.csv")
chart_names1 <- read_csv("https://raw.githubusercontent.com/katiesegreti/infoshare/master/prevention_chart_names.csv")
chart_names <- chart_names1$c_name
boro_choices <- list("Bronx" = "5", 
                     "Brooklyn" = "47",
                     "Manhattan" = "61", 
                     "Queens" = "81", 
                     "Staten Island" = "85")
#function to make a list of zip codes depending on the borough
zip_list <- function(boro) {
  q <- prev %>%
    filter(borough == boro) %>%
    select(area, MapID)
  as.list(set_names(q$MapID, q$area))
}

ui <- dashboardPage(
  dashboardHeader(title=h4("Health Status 2014 - Prevention", align="center")),
  dashboardSidebar(
    uiOutput("borough"),
    uiOutput("zip")
    ),
  dashboardBody(
    uiOutput("what"),
    plotOutput("plot2"))
)

server <- function(input,output){
  output$borough <- renderUI({
    selectInput("borough", label = "Borough: ", choices = boro_choices)
  })
  
  borough_selected <- reactive({
    input$borough
  })
  output$zip <- renderUI({
    selectInput("zip", label = "Zip Code: ", choices = zip_list(borough_selected()))
  })
  zip_selected <- reactive({
    input$zip
  })
  output$what <- renderUI({
    as.character(borough_selected())
  })
  output$plot2<-renderPlot({
    p1 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = cholesterol, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(cholesterol, "%"), y = cholesterol - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[1]
      )
    p2 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = insurance, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(insurance, "%"), y = insurance - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[2]
      )
    p3 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = colonoscopy, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(colonoscopy, "%"), y = colonoscopy - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[3]
      )
    p4 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = mammagrophy, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(mammagrophy, "%"), y = mammagrophy - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[4]
      )
    p5 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = older_men, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(older_men, "%"), y = older_men - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[5]
      )
    p6 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = older_women, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(older_women, "%"), y = older_women - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[6]
      )
    p7 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = pap_smear, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(pap_smear, "%"), y = pap_smear - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[7]
      )
    p8 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = high_bp, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(high_bp, "%"), y = high_bp - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[8]
      )
    p9 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = dentist, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(dentist, "%"), y = dentist - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[9]
      )
    p10 <- prev %>%
      filter(MapID %in% c("1", borough_selected(), zip_selected())) %>%
      ggplot(aes(x = display_name, y = doctor, fill = display_name)) +
      geom_col() +
      scale_fill_manual(values = c("green", "lightblue", "grey")) +
      geom_text(aes(label = paste0(doctor, "%"), y = doctor - 4)) +
      coord_flip() +
      ylim(0, 100) +
      sc_1theme + 
      labs(
        x = "",
        y = "",
        title = chart_names[10]
      )
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)
  }, height = 1200, width = 900)
}
shinyApp(ui, server)


