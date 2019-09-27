server <- function(input, output, session) {
  #################################
  #########REACTIVES###############
  #################################
  ###disporder_at()
  ###areatypes()
  ###sub_area()
  ###area_owner()
  ###areaid()
  ###datfileid()
  ###file0()
  ###nrow_years()
  ###yearid()
  ###catid()
  ###groups()
  ###var_info()
  ###file2()
  ###stacks_name()
  ###fieldnames()
  ###stacks_data()
  ###group_n()
  ###datfile_n()
  ###year_n()
  ###table_title_1
  ###table_1
  
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
  #reactive get var info
  var_info <- reactive({
    req(input$group)
    vars_info_array(input$group)
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
  
  #reactive group name
  group_n <- reactive({
    req(groups_picked())
    groups_picked() %>% map_chr(group_name)
    #group_name(groups_picked())
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
  #reactive make the table
  # table_1 <- reactive({
  #   req(stacks_data())
  #   setNames(data.frame(var_info()$dispname, stacks_data()),
  #            c(table_title_1(), area_name(areaid())))
  # })
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
  #START HERE
  pct_col <- reactive({
    req(stacks_data())
    #stacks_data()[,1] / stacks_total()
    #stacks_data()[,1] %>% map_dbl(function(x) ifelse(x > 0, x / stacks_total(), NA))
    #map(stacks_total(), function(x) map_dbl(stacks_data()$nums, 
    #                                        function(y) ifelse(y > 0, y / x, NA)))
    map2(stacks_data(), stacks_total(), 
         ~ mutate(.x, pct_col = ifelse(nums > 0, nums / .y, NA)))
  })
  #event reactive area column name
  area_column <- eventReactive(input$btn_table, {
    area_name(areaid())
  })
  #source / area detials below chart var_info()[[1]][1,2]
  data_1 <- eventReactive(input$btn_table, {
    setNames(data.frame(var_info()[[1]][,2], abs(stacks_data()[[1]]), pct_col()[[1]][2]),
             c(table_title_1()[1], area_column(), "% of total"))
  })
  data_2 <- eventReactive(input$btn_table, {
    if(length(input$group) > 1) {
        setNames(data.frame(var_info()[[2]][,2], abs(stacks_data()[[2]]), pct_col()[[2]][2]),
             c(table_title_1()[2], area_column(), "% of total"))
      }
  })
  table_source0 <- eventReactive(input$btn_table, {
    "Table Details"
  })
  table_source1 <- eventReactive(input$btn_table, {
    req(input$region)
    paste0("Region: ", "\t", region_name(input$region))
  })
  table_source2 <- eventReactive(input$btn_table, {
    req(input$group)
    paste0("Table: ", group_n(), " - ", datfile_n())
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
  #area / selections above table
  
  
  table_detail <- reactive( {
    req(input$group)
    paste0("Tables: ", group_n(), " - ", datfile_n())
  })
  #######################################
  ############RENDERUIs#################
  #######FOR SIDEBAR SELECTORS#########
  ####################################
  ###areatype_selector
  ###area_sub           (EXISTENCE CONDITIONAL)
  ###area_selector      (ALWAYS THERE CONDITIONAL)
  ###btn_area
  ###filecat
  ###datafile
  ###year               (EXISTENCE CONDITIONAL)
  ###cat
  ###group
  ###whatgroup          TESTING
  
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
                  `dropup-auto` = FALSE,
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
    #req(input$filecat)
    #as.character(input$sidebarItemExpanded)
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
  ###action button
  # observeEvent(input$btn_area, {
  #   toggle("areatype_selector")
  #   toggle("region")
  #   toggle("area_sub")
  #   toggle("area_selector")
  #   
  # })
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
  #try making datfileid a reactive?
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
  output$whatgroup <- renderUI({
    #as.character(stacks_total())
    #catid()
    #paste0(stacks_name(), " ", field_names(), " ", areaid())
    #file0()
    #file2()
    #as.character(stacks_name()[3])
    #as.character(field_names())
    #as.character(pct_col())
    #as.character(sum(stacks_data()[[1]]))
    #str(stacks_total())
    #as.character(stacks_nrow())
    #str(var_info())
    as.character(var_info()[[1]][1,2])
    #str(pct_col())
    #(stacks_nrow())
    #names(stacks_data()[[1]])
    #as.character(stacks_data()[[1]])
    #as.character(file0())
    #p(stacks_name())
    #table_title_1()
    #paste0(group_n(), " - ", datfile_n(), " - ", year_n())
    #table_title_1()
    #input$areatype_selector
    #field_names()
    #as.character(class(stacks_data()[[1]][,1]) )
    #as.character(length(var_info()))
    #as.character(var_info()[[1]][1,2])
    #paste0(groups_picked()[2], " ", stacks_name())
    #datfileid()
  })
  ###action button
  observeEvent(input$btn_table, {
    
    
  })
  #area and data details
  # output$area_detail <- renderUI({
  #   req(input$filecat)
  #   HTML(paste0( "<b>", "Area selected: ", "</b>",  "&nbsp;&nbsp;&nbsp;&nbsp;" ,
  #                areatype_name(input$areatype_selector), 
  #                " - ", area_name(areaid()) ))
  # })
  # output$region_detail <- renderUI({
  #   req(input$filecat)
  #   HTML(paste0("<b>", "Region: ", "</b>",  "&nbsp;&nbsp;&nbsp;&nbsp;" ,
  #               "&nbsp;&nbsp;&nbsp;&nbsp;",
  #               "&nbsp;&nbsp;&nbsp;&nbsp;",
  #               "&nbsp;&nbsp;&nbsp;&nbsp;",
  #               region_name(input$region)))
  # })
  # output$source_detail <- renderUI({
  #   req(input$group)
  #   HTML(paste0("<b>", "Source: ", "</b>", "&nbsp;&nbsp;&nbsp;&nbsp;",
  #               source_name(datfileid())))
  # })
  # output$table_detail <- renderUI({
  #   req(input$group)
  #   HTML(paste0("<b>", "Tables: ", "</b>", "&nbsp;&nbsp;&nbsp;&nbsp;",
  #               group_n()))
  # })
  
  #####TABLES
  
  output$table_1 <- DT::renderDataTable({
    DT::datatable(data = data_1(), 
                  options = list(pageLength = 10,
                                 lengthMenu = list(c(10, 25, 100, -1), 
                                                   c("10", "25", "100", "All"))),
                  rownames = FALSE) %>% 
      #DT::formatRound(area_column(), 0) %>%
      DT::formatPercentage('% of total', digits =0) %>%
      DT::formatStyle('% of total',
                      background = DT::styleColorBar(range(data_1()[3], na.rm = TRUE),
                                                     color ='#EFDA5B',
                                                     angle = -90),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
  })
  output$table_2 <- DT::renderDataTable({
      req(data_2())
        DT::datatable(data = data_2(), 
                    options = list(pageLength = 10,
                                   lengthMenu = list(c(10, 25, 100, -1), 
                                                     c("10", "25", "100", "All"))),
                    rownames = FALSE) %>% 
        #DT::formatRound(area_column(), 0) %>%
        DT::formatPercentage('% of total', digits =0) %>%
        DT::formatStyle('% of total',
                        background = DT::styleColorBar(range(data_2()[3], na.rm = TRUE),
                                                       color ='#EFDA5B',
                                                       angle = -90),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
       
  })
  output$table_source0 <- renderText(table_source0())
  output$table_source1 <- renderText(table_source1())
  output$table_source2 <- renderText(table_source2())
  output$table_source3 <- renderText(table_source3())
  output$table_source4 <- renderText(table_source4())
}

