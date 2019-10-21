server <- function(input, output, session) {
  #################################
  #########REACTIVES###############
  #################################
  #reactive get disporder
  disporder_at <- reactive({
    req(input$areatype_selector)
    as.character(at_disporder2(input$areatype_selector))
  })
  #reactive get areatypes 
  areatypes <- reactive({
    req(input$region)
    # with(get_areatypes(input$region), 
    #      split(c(areatypeid, dispname), fct_inorder(factor(disporder))))
    get_areatypes(input$region) %>%
      mutate(slash = str_detect(dispname, regex("^\\\\"))) %>%
      mutate(steez = ifelse(slash == TRUE, "color: black; font-weight: bold;", "color: black;"))
  })
  #reactive get sub area type
  sub_area <- reactive({
    req(input$area2)
    if(input$region == "NYC") {
      "NYCCOUNTIES"
    } else if(input$region == "NYS") {
      "NYSCOUNTIES"
    }
    else if(input$region == "USA") {
      "USASTATES"
    }
  })
  #reactive get owner 
  area_owner <- reactive({
    req(input$area2)
    selected_area(input$area2, sub_area())
  })
  #reactive get areaid
  areaid <- reactive({
    #req(input$area_selector)
    if(disporder_at() == 101) {
      get_areas(input$areatype_selector)[1,1]
    }else{
      input$area_selector
    }
  })
  
  #reactive get datfileid
  datfileid <- reactive({
    req(input$datfile)
    input$datfile
  })
  #reactive get file0
  file0 <- reactive({
    req(datfileid())
    get_file0(datfileid())[1,1]
  })
  #reactive number of years
  nrow_years <- reactive({
    req(datfileid())
    nrow(get_years(datfileid()))
  })
  #reactive get yearid
  yearid <- reactive({
    if(nrow_years() > 1) {
      input$year
    } else if (nrow_years() == 1){
      get_years(input$datfile)[1,1]
    }
    else {""}
  })
  #reactive get catid 
  catid <- reactive({
    if(nrow(get_cats(yearid())) > 1) {
      input$cat
    } else if (nrow(get_cats(yearid())) == 1) {
      get_cats(yearid())[,1]
    }
  })
  #reactive get groups list
  groups <- reactive({
    get_groups(catid()) %>%
      mutate(slash = str_detect(dispname, regex("^\\\\"))) %>%
      mutate(steez = ifelse(slash == TRUE, "color: black; background: lightgrey; font-weight: bold;", "color: black;"))
  })
  #reactive get var info (switch to the reactive group)
  var_info <- reactive({
    # req(input$group)
    # vars_info_array(input$group)
    if(length(rv$gruppi) > 0) {
      vars_info_array(rv$gruppi)
    }
  })
  #reactive get file1 from var_info
  file1 <- reactive({
    req(var_info())
    var_info() %>% map_chr(function(x) trimws(x[1,5]))
  })
  #reactive get file2 from var_info
  file2 <- reactive({
    req(var_info())
    var_info() %>% map_chr(function(x) trimws(x[1,3]))
  })
  #reactive stacks table name
  stacks_name <- reactive({
    req(datfileid())
    map2_chr(file1(), file2(), ~ paste0(trimws(file0()), .x, .y))
    #as.character(paste0(trimws(file0()), file1(), file2()))
  })
  #reactive field names string
  field_names <- reactive({
    req(var_info())
    var_info() %>% map_chr(function(x) str_c(trimws(x$field2), collapse = ", "))
    #str_c(trimws(var_info()$field2), collapse = ", ")
  })
  #reactive get the data
  #why doesn't stacks_name() work here? SOLVED
  #IT WAS A DATA FRAME NOT A SINGLE CHARACTER
  stacks_data <- reactive({
    req(var_info())
    map2(stacks_name(), field_names(), 
         ~ setNames(get_stacks_data(.x, .y, areaid()), "nums"))
    
    #as.data.frame(get_stacks_data(stacks_name(), field_names(), areaid())) 
  })
  
  #make a reactive groups selected
  groups_picked <- reactive({
    req(input$group)
    input$group
  })
  #reactive values to save the groups
  rv <- reactiveValues(gruppi=c())
  #reactive group name
  group_n <- reactive({
    if(length(rv$gruppi) >0) {
      rv$gruppi %>% map_chr(group_name)
    }
    # req(groups_picked())
    # groups_picked() %>% map_chr(group_name)
    
  })
  #reactive datfile name
  datfile_n <- reactive({
    req(datfileid())
    datfile_name(datfileid())
  })
  #reactive year name
  year_n <- reactive({
    req(yearid())
    year_name(yearid())
  })
  #reactive table title 1 name
  table_title_1 <- reactive({
    group_n() %>% map_chr(function(x) paste0(x, " - ", datfile_n(), " - ", year_n()))
    #paste0(group_n(), " - ", datfile_n(), " - ", year_n())
  })
  #reactive get total value from stacks data
  stacks_total <- reactive({
    req(stacks_data())
    stacks_data() %>% map_dbl(function(x) sum(x[,1][x[,1]>0]))
    #sum(stacks_data()[[1]][,1][stacks_data()[[1]][,1]>0])
  })
  #reactive number of columns in stacks_data
  stacks_nrow <- reactive({
    req(stacks_data())
    #as.character(nrow(stacks_data()))
    stacks_data() %>% map_int(function(x) nrow(x))
  })
  #reactive % of total for table
  pct_col <- reactive({
    req(stacks_data())

    map2(stacks_data(), stacks_total(), 
         ~ mutate(.x, pct_col = ifelse(nums > 0, nums / .y, NA)))
  })
  #reactive integers for length of rv$gruppi
  num_tables <- reactive({
    if(length(rv$gruppi) > 0) {
      1:length(rv$gruppi)
    } else {NA}
  })
  ################################
  ###EVENT REACTIVES
  ################################
  #event reactive area column name
  area_column <- eventReactive(input$btn_table, {
    area_name(areaid())
  })

  #eventreactive data tables
  data_tbls <- eventReactive(input$btn_table, {
    num_tables() %>% map(function(x) setNames(data.frame(var_info()[[x]][,2], abs(stacks_data()[[x]]),
                                                         pct_col()[[x]][2]),
                                              c(table_title_1()[x], area_column(), "% of total")))
  })
  
  table_source0 <- eventReactive(input$btn_table, {
    if(length(rv$gruppi > 0)) {"Table Details"}
  })
  table_source1 <- eventReactive(input$btn_table, {
    req(input$region)
    paste0("Region: ", "\t", region_name(input$region))
  })
  table_source3 <- eventReactive(input$btn_table, {
    req(datfileid())
    paste0("Source: ", source_name(datfileid()))
  })
  table_source4 <- eventReactive(input$btn_table, {
    req(areaid())
    paste0("Area Profiled: ", areatype_name(input$areatype_selector), 
           " - ", area_name(areaid()))
  })

  #reactive dataframe for table source, to bind to tbls for download
  table_df <- reactive({
    as.data.frame(c(table_source0(), table_source1(), table_source3(), table_source4()))
  })
  
  #######################################
  ############RENDERUIs#################
  #######FOR SIDEBAR SELECTORS#########
  ####################################

  # renderui select input for areatype
  output$areatype_selector <- renderUI({
    # selectInput("areatype_selector", "Area Type: ", 
    #             choices = with(get_areatypes(input$region), 
    #                           split(areatypeid, fct_inorder(factor(dispname)))))
    pickerInput("areatype_selector",
                label = "Area Type: ",
                choices = with(areatypes(), 
                               split(areatypeid, 
                                     fct_inorder(str_remove_all(factor(dispname), regex("^\\\\"))))),
                #multiple = TRUE,
                
                options = list(
                  `actions-box` = TRUE,
                  `dropup-auto` = TRUE,
                  `selected-text-format`= "count",
                  title = "Area type: "),
                choicesOpt = list(
                  disabled = str_detect(areatypes()[[2]], regex("^\\\\")),
                  style = areatypes()[[6]]
                ))
  })
  # renderui select input for state, county or borough (CONDITIONAL)
  output$area_sub <- renderUI({
    req(input$areatype_selector)
    if(as.integer(disporder_at()) < 100) {
      if(input$region == "USA") {
        selectInput("area2", "State: ",
                    choices = with(get_areas("USASTATES"), 
                                   split(areaid, fct_inorder(factor(dispname)))))
      } else if(input$region == "NYS") {
        selectInput("area2", "County: ",
                    choices = with(get_areas("NYSCOUNTIES"), 
                                   split(areaid, fct_inorder(factor(dispname)))))
      } else if(input$region == "NYC") {
        radioButtons("area2", "Borough: ",
                     choices = with(get_areas("NYCCOUNTIES"), 
                                    split(areaid, fct_inorder(factor(dispname)))),
                     inline = TRUE)
      }
    }
  })
  # renderui select input for  area (ALWAYS THERE CONDITIONAL)
  output$area_selector <- renderUI({
    #req(input$areatype_selector)
    if( (as.integer(disporder_at()) < 100)) {
      selectInput("area_selector", label = 
                    paste(areatype_name(input$areatype_selector), ":"),
                  choices = with(get_areas2(input$areatype_selector, area_owner()),
                                 split(areaid, fct_inorder(factor(dispname)))))
    } else if(as.integer(disporder_at()) == 1001) {
      selectInput("area_selector", label = 
                    paste(areatype_name(input$areatype_selector), ":"),
                  choices = with(get_areas(input$areatype_selector),
                                 split(areaid, fct_inorder(factor(dispname)))))
    }
    
  })
  ### show area selected
  output$show_area <-  renderUI({
    if(!is.null(input$sidebarItemExpanded) & length(input$sidebarItemExpanded) > 0) {
      if(input$sidebarItemExpanded != "1.Selectarea:"  ) {
        HTML(paste0( "<p>", "Region:  &nbsp;&nbsp;&nbsp;&nbsp;",
                     region_name(input$region),
                     "&nbsp;&nbsp;&nbsp;&nbsp; ", "<br>", "Area: ",
                     "&nbsp;&nbsp;&nbsp;&nbsp;" ,
                     areatype_name(input$areatype_selector),
                     " - ", area_name(areaid()), "</p>"))
      }else{}
    }else {HTML(paste0( "<p>", "Region: &nbsp;&nbsp;&nbsp;&nbsp;",
                        region_name(input$region),
                        "&nbsp;&nbsp;&nbsp;&nbsp;", "<br>", "Area: ",
                        "&nbsp;&nbsp;&nbsp;&nbsp;" ,
                        areatype_name(input$areatype_selector),
                        " - ", area_name(areaid()), "</p>")) }
  })

  #filecat (demographic, health, etc)
  output$filecat <- renderUI({
    req(input$region)
    radioButtons("filecat", "Select data source: ",
                 choices = with(get_filecats(input$region), split(filecatid, 
                                                                  fct_inorder(factor(dispname)))),
                 inline = TRUE)
  })
  #datafile (census, community survey, etc)
  output$datafile <- renderUI({
    req(input$filecat)
    selectInput("datfile", label = NULL,
                choices = with(get_datafiles(input$filecat), 
                               split(datfileid, fct_inorder(factor(dispname)))))
  })
  #year (sometimes there's only one so it won't show up?)
  output$year <- renderUI({
    req(datfileid())
    #datfileid()
    paste0(as.character(nrow(get_years(datfileid()))), datfileid())
    if(nrow(get_years(datfileid())) > 1)  {
      selectInput("year", label = "Select year: ",
                  choices = with(get_years(datfileid()), 
                                 split(yearid, fct_inorder(factor(dispname)))))
    } else{}
  })
  ### show source selected
  output$show_source <-  renderUI({
    req(yearid())
    
    if(!is.null(input$sidebarItemExpanded) & length(input$sidebarItemExpanded) > 0) {
      if(input$sidebarItemExpanded != "2.Selectdatasource:"  ) {
        HTML(paste0("<p>", "Source: ",  "<br>",
                    source_name1(datfileid()), "</p>"))
      }else{}
    }else {HTML(paste0("<p>", "Source: ",  "<br>",
                       source_name1(datfileid()), "</p>")) }
  })
  
  #radio buttons for cats if applicable
  output$cat <- renderUI({
    req(yearid())
    #as.character(nrow(get_cats(yearid())))
    
    #keep this in output$cat
    if(nrow(get_cats(yearid())) > 1) {
      radioButtons("cat", "Category: ",
                   choices = with(get_cats(yearid()), 
                                  split(catid, fct_inorder(factor(dispname)))),
                   inline = TRUE)
    }
  })
  #select input for groups
  output$group <- renderUI({
    pickerInput("group",
                label = "Select data: ",
                choices = with(groups(), split(groupid, 
                                               fct_inorder(str_replace_all(factor(dispname), regex("^\\\\"), " ")))),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `dropup-auto` = FALSE,
                  `selected-text-format`= "count",
                  title = "Select data"),
                choicesOpt = list(
                  disabled = str_detect(groups()[[2]], regex("^\\\\")),
                  style = groups()[[4]]
                ))
  })
  #################TESTING SECTION###########
  output$whatgroup <- renderUI({

    as.character(length(num_tables()))
    
  })
  ######################
  ##RENDERUI FOR TABS
  #######################
  output$table_tabs <- renderUI({
    nTabs = length(rv$gruppi)
    myTabs = map(seq_len(nTabs), function(x)
      if(length(rv$gruppi) >= x) {
        tabPanel(
          title = group_n()[x],
          br(),
          DT::dataTableOutput(outputId = paste0("table_", x)),
          br(), br()
        )
      }
      )
    do.call(tabsetPanel, myTabs)
  })
  # output$table_tabs <- renderUI({
  #   if(!(is.null(data_tbls()[[1]]))) {  
  #     tabsetPanel(
  #      tabPanel(
  #        title = group_n()[1],
  #        br(),
  #        DT::dataTableOutput(outputId = "table_1"),
  #        br(), br()
  #      ),
  #      #uiOutput(ab")"first_t,
  #      if(length(rv$gruppi) > 1) {
  #        tabPanel(
  #          title = group_n()[2],
  #          br(),
  #          DT::dataTableOutput(outputId = "table_2"),
  #          br(), br()
  #        )
  #      },
  #      #tab 3
  #      if(length(rv$gruppi) > 2) {
  #        tabPanel(
  #          title = group_n()[3],
  #          br(),
  #          DT::dataTableOutput(outputId = "table_3"),
  #          br(), br()
  #        )
  #      }
  #     )
  #   }
  # })
  
  ###############################
  ###ACTION BUTTONS
  ###################################
  observeEvent(input$btn_clear, {
    rv$gruppi <- c()
    
  })
  observeEvent(input$btn_table, {
    rv$gruppi <- c(rv$gruppi, groups_picked()) %>% unique()
    
  })
  observeEvent(input$group, {
    rv$gruppi <- c(rv$gruppi, groups_picked()) %>% unique()
  })
  
  ###################################
  #####TABLES
  #####CREATE TABLES WITH RENDERTABLE
  
  #try to map this! using observe?
  observe(
    num_tables() %>% map(function(x) 
      output[[paste0("table_", x)]] <- DT::renderDataTable({
        DT::datatable(data = data_tbls()[[x]],
                      options = list(pageLength = 10,
                                     lengthMenu = list(c(10, 25, 100, -1),
                                                       c("10", "25", "100", "All"))),
                      rownames = FALSE) %>%
          DT::formatPercentage('% of total', digits = 0) %>%
          DT::formatStyle('% of total',
                          background = DT::styleColorBar(range(data_tbls()[[x]][3], na.rm = TRUE),
                                                         color = '#EFDA5B',
                                                         angle = -90),
                          backgroundSize = '98% 88%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center')
      }))
  )
  
  # 
  # output$table_1 <- DT::renderDataTable({
  #   DT::datatable(data = data_tbls()[[1]], 
  #                 options = list(pageLength = 10,
  #                                lengthMenu = list(c(10, 25, 100, -1), 
  #                                                  c("10", "25", "100", "All"))),
  #                 rownames = FALSE) %>% 
  #     #DT::formatRound(area_column(), 0) %>%
  #     DT::formatPercentage('% of total', digits =0) %>%
  #     DT::formatStyle('% of total',
  #                     background = DT::styleColorBar(range(data_tbls()[[1]][3], na.rm = TRUE),
  #                                                    color ='#EFDA5B',
  #                                                    angle = -90),
  #                     backgroundSize = '98% 88%',
  #                     backgroundRepeat = 'no-repeat',
  #                     backgroundPosition = 'center')
  #     
  # })
  
    # output$table_2 <- DT::renderDataTable({
    #   req(data_tbls()[[2]])
    #   DT::datatable(data = data_tbls()[[2]], 
    #                 options = list(pageLength = 10,
    #                                lengthMenu = list(c(10, 25, 100, -1), 
    #                                                  c("10", "25", "100", "All"))),
    #                 rownames = FALSE) %>% 
    #     #DT::formatRound(area_column(), 0) %>%
    #     DT::formatPercentage('% of total', digits =0) %>%
    #     DT::formatStyle('% of total',
    #                     background = DT::styleColorBar(range(data_tbls()[[2]][3], na.rm = TRUE),
    #                                                    color ='#EFDA5B',
    #                                                    angle = -90),
    #                     backgroundSize = '98% 88%',
    #                     backgroundRepeat = 'no-repeat',
    #                     backgroundPosition = 'center')
    #   
    # })
    # 
    
  # output$table_3 <- DT::renderDataTable({
  #   req(data_tbls()[[3]])
  #   DT::datatable(data = data_tbls()[[3]], 
  #                 options = list(pageLength = 10,
  #                                lengthMenu = list(c(10, 25, 100, -1), 
  #                                                  c("10", "25", "100", "All"))),
  #                 rownames = FALSE) %>% 
  #     #DT::formatRound(area_column(), 0) %>%
  #     DT::formatPercentage('% of total', digits =0) %>%
  #     DT::formatStyle('% of total',
  #                     background = DT::styleColorBar(range(data_tbls()[[3]][3], na.rm = TRUE),
  #                                                    color ='#EFDA5B',
  #                                                    angle = -90),
  #                     backgroundSize = '98% 88%',
  #                     backgroundRepeat = 'no-repeat',
  #                     backgroundPosition = 'center')
  #   
  # })
  ####################################
  ######RENDER TEXT FOR TABLE SOURCE
  #########################################
  output$table_source0 <- renderText(table_source0())
  output$table_source1 <- renderText(table_source1())
  #output$table_source2 <- renderText(table_source2())
  output$table_source3 <- renderText(table_source3())
  output$table_source4 <- renderText(table_source4())

########################################
###DOWNLOAD BUTTON
  #############################
  output$download_data <- downloadHandler(
    filename = function() {
      paste("infoshare", Sys.Date(), ".xls", sep ="")
    },
    content = function(file) {
      #write.csv(data_1(), file)
      write.xlsx(data_tbls()[[1]], file, sheetName = table_title_1()[1], row.names = FALSE)
      if(length(rv$gruppi) > 1) {
        write.xlsx(data_tbls()[[2]], file, sheetName = table_title_1()[2], append = TRUE, row.names = FALSE)
      }
      write.xlsx(table_df(), file, sheetName = "table details", append = TRUE, row.names = FALSE, col.names = FALSE)
    
    }
  )
}

