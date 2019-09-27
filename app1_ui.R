library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(
    title="INFOSHARE"
  ),
  #####################################################
  ########################SIDEBAR#####################
  #####################################################
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    width = "275px",
   
    useShinyjs(),
    sidebarMenu(id = "menu1",
                
      #select region
      br(),
      uiOutput("show_area"),
      uiOutput("show_source"),
      menuItem("1. Select area:", tabName = "regionareaMenu",
               startExpanded = TRUE,
               selectInput(inputId = "region",
                           label = "Region: ",
                           choices = region_choices),
               #select areatype
               uiOutput("areatype_selector"),
               # area_sub: state , county or borough selection when applicable
               uiOutput("area_sub"),
               #select area
               uiOutput("area_selector")
      ),
      
      menuItem("2. Select data source:", tabName = "sourceMenu",
               #select filecats (demographics, s-e, health)
               uiOutput("filecat"),
               #select datafile (census, community survey, etc)
               uiOutput("datafile"),
               #select year
               uiOutput("year")
      ),
      
      menuItem("3. Select data: ", tabName = "dataselectMenu",
               #select cat (population, housing, work school etc)
               uiOutput("cat"),
               #select group
               uiOutput("group"),
               #what group?
               uiOutput("whatgroup")
      ),
      actionButton("btn_table", "Display table")
    )
  ),
  ###############################################
  ######################OUTPUT###################
  ###############################################
  dashboardBody(
    
    h1("Area Profile"),
    
    # htmlOutput(outputId = "area_detail"),
    # htmlOutput(outputId = "region_detail"),
    # htmlOutput(outputId = "source_detail"),
    # htmlOutput(outputId = "table_detail"),
    br(),
    # TABLES GO HERE
    #these have to be renderui dependent on table1, table2 etc
    DT::dataTableOutput(outputId = "table_1"),
    br(), br(),
    DT::dataTableOutput(outputId = "table_2"),
    downloadButton(outputId = "download_data", label = "Download data"),
    br(), br(),
    textOutput(outputId = "table_source0"),
    textOutput(outputId = "table_source1"),
    textOutput(outputId = "table_source2"),
    textOutput(outputId = "table_source3"),
    textOutput(outputId = "table_source4")
  )
)
