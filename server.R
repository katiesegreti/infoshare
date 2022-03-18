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
  #reactive selected tables t/f
  selected_datfiles <- reactive({
    req(datfileid())
    if(datfileid() %in% datfiles_with_selected) {
      TRUE
    } else { FALSE }
  })
  #reactive get file0 (use rv$gruppi[,2] instead of datfileid() i think)
  file0 <- reactive({
    #req(datfileid())
    #get_file0(datfileid())[1,1]
    if(nrow(rv$gruppi) > 0) {
      rv$gruppi[,2] %>% map_chr(function(x) get_file0(x))
    }
    
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
    get_groups(catid(), selected_datfiles()) %>%
      mutate(slash = str_detect(dispname, regex("^\\\\"))) %>%
      mutate(steez = ifelse(slash == TRUE, "color: black; background: lightgrey; font-weight: bold; font-size: 12px", "color: black; font-size: 12px"))
  })
  #reactive get var info (switch to the reactive group)
  var_info <- reactive({
    # req(input$group)
    # vars_info_array(input$group)
    if(nrow(rv$gruppi) > 0) {
      vars_info_array(rv$gruppi[,1])
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
    #req(datfileid())
    pmap(list(x = file1(), y = file2(), z = file0()),
         .f = function(x, y, z) { paste0(trimws(z), x, y)})
    #map2_chr(file1(), file2(), ~ paste0(trimws(file0()), .x, .y))
    #as.character(paste0(trimws(file0()), file1(), file2()))
  })
  #reactive field names string
  field_names <- reactive({
    req(var_info())
    var_info() %>% map_chr(function(x) str_c(trimws(x$field2), collapse = ", "))
    #str_c(trimws(var_info()$field2), collapse = ", ")
  })
  #reactive get the data
  stacks_data <- reactive({
    req(var_info())
    map2(stacks_name(), field_names(), 
         ~ setNames(get_stacks_data(.x, .y, areaid()), "nums"))
  })
  
  #make a reactive groups selected
  groups_picked <- reactive({
    req(input$group)
    input$group
  })
  ######REACTIVE VALUES
  #reactive values to save the groups
  rv <- reactiveValues()
  rv$gruppi <-data.frame(group = character(), source = character(), year = character())
  #reactive values for tooltips
  rv$tips <- c()
  observe({
    num_tables() %>% map(function(x) 
      #rv[[paste0("tip_", x)]] <- table_source3()[x])
      rv$tips[x] <- paste0(datfile_name(rv$gruppi[x,2]), " - ", year_name(rv$gruppi[x,3])))
  })
  #reactive values for tables (groups) to remove
  rv$rm_tables <- c()
  
  #reactive values of moe_checkboxes
  rv$moe <- c()
  observe({
   num_tables() %>% map(function(x)
     if(nrow(rv$gruppi) > 0) {
       if(include_moe()[x] == TRUE) {
         rv$moe[x] <- ifelse(is.null(input[[paste0("moe_checkbox_", x)]]), TRUE, input[[paste0("moe_checkbox_", x)]])
       } else {
         rv$moe[x] <- FALSE
       }
     } 
     )
  })
  #reactive values of pct_checkboxes
  rv$pct <- c()
  observe({
    num_tables() %>% map(function(x)
      if(nrow(rv$gruppi) > 0) {
        if(no_pct_col()[x] == FALSE) {
          rv$pct[x] <- ifelse(is.null(input[[paste0("pct_checkbox_", x)]]), TRUE, input[[paste0("pct_checkbox_", x)]])
        } else {
          rv$pct[x] <- FALSE
        }
      } 
    )
  })
  
  #I THINK I NEED TO MAKE THE data_tbls() RV
  #https://stackoverflow.com/questions/50251813/how-to-update-datatable-in-shiny-with-button
  # rv$data_tbls1 <- list()
  # observe({
  #  num_tables() %>% map(function(x)
  #    if(no_pct_col()[x] == FALSE) { 
  #      if(include_moe()[x] == TRUE) {
  #       rv$data_tbls1[[x]] <- setNames(data.frame(var_info()[[x]][,2], abs(stacks_data()[[x]]),
  #                            pct_col()[[x]][2], moe_column()[[x]]),
  #                 c(table_title_1()[x], area_column(), "% of total", "margin of error"))
  #      }else {
  #        rv$data_tbls1[[x]] <- setNames(data.frame(var_info()[[x]][,2], abs(stacks_data()[[x]]),
  #                            pct_col()[[x]][2]),
  #                 c(table_title_1()[x], area_column(), "% of total"))
  #      }
  #    } else {
  #      rv$data_tbls1[[x]] <- setNames(data.frame(var_info()[[x]][,2], abs(stacks_data()[[x]])),
  #               c(table_title_1()[x], area_column()))
  #    }
  #    )
  # })
  
  #reactive group name
  group_n <- reactive({
    if(nrow(rv$gruppi) >0) {
      rv$gruppi[,1] %>% map_chr(group_name)
    }
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
  #REDO THIS WITH ARRAYS FOR DATFILE_N AND YEAR_N
  table_title_1 <- reactive({
    #group_n() %>% map_chr(function(x) paste0(x, " - ", datfile_n(), " - ", year_n()))
    pmap(list(x = group_n(), y = rv$gruppi[,2], z = rv$gruppi[,3]),
         .f = function(x, y, z) { paste0(x, " - ", datfile_name(y), " - ", year_name(z))})
    #paste0(group_n(), " - ", datfile_n(), " - ", year_n())
  })
  #reactive get total value from stacks data
  stacks_total <- reactive({
    req(stacks_data())
    stacks_data() %>% map_dbl(function(x) sum(x[,1][x[,1]>0]))
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
    if(nrow(rv$gruppi) > 0) {
      1:nrow(rv$gruppi)
    } else {NA}
  })
  #nopctcol reactive to indicate pct col not a
  no_pct_col <- reactive({
    req(pct_col())
    num_tables() %>% map(function(x) ifelse(all(is.na(pct_col()[[x]][2])), TRUE, FALSE))
  })
  #include_moe reactive to indicate if there's a margin of error to include
  include_moe <- reactive({
    if(nrow(rv$gruppi) > 0) {
      1:nrow(rv$gruppi) %>% map(function(x) ifelse(rv$gruppi[x,2] %in% datfiles_with_moe, TRUE, FALSE))
    }
  })
  #reactive moe column
  moe_column <- reactive({
    req(var_info())
    map2(stacks_name(), field_names(), 
         ~ setNames(get_moe(.x, .y, areaid()), "moe"))
  })
  
  #yes_show_moe to show moe if applicable, if it's selected
  yes_show_moe <- reactive({
    if(nrow(rv$gruppi) > 0) {
      1:nrow(rv$gruppi) %>% map(function(x) ifelse(include_moe()[x] == TRUE, FALSE, NA))
    }
  })
  
  #reactive area column name
  area_column <- reactive({
    area_name(areaid())
  })


  #reactive data tables (THIS SHOULD BE RV? WTIH LOGIC ON THE COLUMNS TO INCLUDE?)
  data_tbls <- reactive({
    num_tables() %>% map(function(x)
      setNames(data.frame(var_info()[[x]][,2], abs(stacks_data()[[x]]),
                          pct_col()[[x]][2], moe_column()[[x]]),
               c(table_title_1()[x], area_column(), "% of total", "margin of error"))

      )
  })
  #I think these should be reactive and not eventReactive?
  table_source0 <- reactive({
    if(nrow(rv$gruppi) > 0) {"Table Details"}
  })
  # table_source1 <- reactive({
  #   req(input$region)
  #   paste0("Region: ", "\t", region_name(input$region))
  # })
  #this one needs to be an array, so map
  table_source3 <- reactive({
    req(datfileid())
    #paste0("Source: ", source_name(datfileid()))
    #rv$gruppi[,2] %>% map(function(x) paste0("Source: ", source_name(x)))
    map2(rv$gruppi[,2], rv$gruppi[,3],
         ~ paste0("Source: ", source_name(.x), " - ", year_name(.y)))
  })
  table_source4 <- reactive({
    req(areaid())
    paste0("Area Profiled: ", areatype_name(input$areatype_selector), 
           " - ", area_name(areaid()))
  })

  #unique list of sources selected
  sources_selected <- reactive({
    rv$gruppi[,2] %>% map_df(function(x) data.frame(s_id = as.character(x), 
                                                    s_n = source_name1(x), stringsAsFactors = FALSE)) %>% 
      unique()
  })
  #number of sources
  num_sources <- reactive({
    if(nrow(sources_selected()) > 0) {
      1:nrow(sources_selected())
    } else {NA}
  })
  #list of tables per source
  #work on this because the group is still factors! :< 
  tables_per_source <- reactive({
    if(nrow(rv$gruppi) > 0) {
      #"hsdfasdf"
      sources_selected()[,1] %>% map(function(x)
        rv$gruppi %>% filter(source == x) %>% select(group) #%>% map(function(x)
           # group_n()
           # )
        )
    }
  })
  #reactive dataframe for table source, to bind to tbls for download
  # table_df <- reactive({
  #   as.data.frame(c(table_source0(), table_source1(), table_source3(), table_source4()))
  # })
  
  #######################################
  ############RENDERUIs#################
  #######FOR SIDEBAR SELECTORS#########
  ####################################
  #renderui warning for changing region and area!
  output$warning <- renderUI({
    if(nrow(rv$gruppi) > 0) {
      HTML(paste("Warning: if you change the region or area,", "your existing tables will be cleared.", sep="<br/>"))
     } 
    
  })
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
                  `actions-box` = FALSE,
                  `dropup-auto` = TRUE,
                  `selected-text-format`= "count",
                  #size = 4,
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
                                   split(areaid, fct_inorder(factor(dispname)))),
                    multiple=TRUE, selectize=FALSE)
      } else if(input$region == "NYS") {
        selectInput("area2", "County: ",
                    choices = with(get_areas("NYSCOUNTIES"), 
                                   split(areaid, fct_inorder(factor(dispname)))),
                    multiple=TRUE, selectize=FALSE)
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
                                 split(areaid, fct_inorder(factor(dispname)))),
                  multiple=TRUE, selectize=FALSE)
    } else if(as.integer(disporder_at()) == 1001) {
      selectInput("area_selector", label = 
                    paste(areatype_name(input$areatype_selector), ":"),
                  choices = with(get_areas(input$areatype_selector),
                                 split(areaid, fct_inorder(factor(dispname)))),
                  multiple=TRUE, selectize=FALSE)
    }
    
  })
  ### show area selected
  output$show_area <-  renderUI({
        HTML(paste0( "<p>", "Region:  &nbsp;&nbsp;&nbsp;&nbsp;",
                     region_name(input$region),
                     "&nbsp;&nbsp;&nbsp;&nbsp; ", "<br>", "Area: ",
                     "&nbsp;&nbsp;&nbsp;&nbsp;" ,
                     areatype_name(input$areatype_selector),
                     " - ", area_name(areaid()), "</p>"))
  })

  #####make a reactive menu and then call that in the renderMenu?
  make_area_menu <- reactive({
    menuItem("1. Select area:", tabName = "regionareaMenu",
             startExpanded = TRUE,
             uiOutput("warning"),
             selectInput(inputId = "region",
                         label = "Region: ",
                         choices = region_choices,
                         multiple=TRUE, selectize=FALSE),
             #select areatype
             uiOutput("areatype_selector"),
             # area_sub: state , county or borough selection when applicable
             uiOutput("area_sub"),
             #select area
             uiOutput("area_selector")
    )
  })
  
  make_source_menu <- reactive({
    if(!is.null(input$area_selector) | req(input$areatype_selector) %in% c("NYCCITY", "NYSSTATE", "USANATION")) {
      menuItem("2. Select data source:", tabName = "sourceMenu",
        
               #select filecats (demographics, s-e, health)
               #as.character(is.null(areaid()))
               uiOutput("filecat"),
               #select datafile (census, community survey, etc)
               uiOutput("datafile"),
               #select year
               uiOutput("year")
      )
    } else {
      menuItem("2. Select data source:", tabName = "sourceMenu",
               "Please select an area first"
      )
    }
  })
  ###reactive menu for group
  make_group_menu <- reactive({
    req(input$areatype_selector)
    if(!is.null(req(input$datfile))) {
      menuItem("3. Select tables: ", tabName = "dataselectMenu",
               #select cat (population, housing, work school etc)
               uiOutput("cat"),
               #select group
               uiOutput("display_button"),
               uiOutput("group")
               
      )
    } else {
      menuItem("3. Select data: ", tabName = "dataselectMenu",
               "\tPlease select area and source first"
      )
    }
  })
  
  ########source menu item renderMenu
  output$regionareaMenu <- renderMenu({
    make_area_menu()
    
  })
  output$sourceMenu <- renderMenu({
    make_source_menu()
    
  })
  #########group menu renderMenu
  output$dataselectMenu <- renderMenu({
    make_group_menu()
  })

  
  #filecat (demographic, health, etc)
  output$filecat <- renderUI({
    req(input$region)
    radioButtons("filecat", "Type of data: ",
                 choices = with(get_filecats(input$region), split(filecatid, 
                                                                  fct_inorder(factor(dispname)))),
                 inline = TRUE)
  })
  #datafile (census, community survey, etc)
  output$datafile <- renderUI({
    req(input$filecat)
    selectInput("datfile", label = NULL, width = '100%',
                choices = with(get_datafiles(input$filecat), 
                               split(datfileid, fct_inorder(factor(dispname)))),
                multiple=TRUE, selectize=FALSE)
  })
  #year (sometimes there's only one so it won't show up?)
  output$year <- renderUI({
    req(datfileid())
    #datfileid()
    paste0(as.character(nrow(get_years(datfileid()))), datfileid())
    if(nrow(get_years(datfileid())) > 1)  {
      selectInput("year", label = "Select year: ",
                  choices = with(get_years(datfileid()), 
                                 split(yearid, fct_inorder(factor(dispname)))),
                  multiple=TRUE, selectize=FALSE)
    } else{}
  })
  ### show source selected
  output$show_source <-  renderUI({
    req(yearid())
    
    if(!is.null(input$sidebarItemExpanded) & length(input$sidebarItemExpanded) > 0) {
      #if(input$sidebarItemExpanded != "2.Selectdatasource:"  ) {
        HTML(paste0("<p>", "Source: ",  "<br>",
                    source_name1(datfileid()), "</p>"))
      #}else{}
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
                selected = NULL,
                options = list(
                  `actions-box` = FALSE,
                  `dropup-auto` = FALSE,
                  `selected-text-format`= "count",
                  width = '390px',
                  title = "Select data"),
                choicesOpt = list(
                  disabled = str_detect(groups()[[2]], regex("^\\\\")),
                  style = groups()[[4]]
                ))
    
  })
  
  #################TESTING SECTION###########
  output$whatgroup <- renderUI({
    str(file0())
    
  })
  ###############RENDERUIS FOR DISPLAY AND CLEAR TABLES
  #####################################################
  
  output$clear_button <- renderUI({
    if(!(is.null(data_tbls()[[1]]))) {
      actionButton("btn_clear", "Clear selected tables")
    }
  })
  #show selected tables 
  #start by getting unique sources used in table selection
  #a checkbox for each source currently used
  #a button to delete selected tables
  #msg that comes up to select tables to delete
  output$delete_tables_msg <- renderUI({
    if(nrow(rv$gruppi) > 0) {
      y <- paste("<br><br>", "Tables selected. Check box to remove tables:", "<br>")
      HTML(y)
      }
    })
  #remove selected tables from rv$gruppi

  #create checkboxes for removing tables
  output$all_checkboxes <- renderUI({
    if(nrow(rv$gruppi) > 0) {
      checkbox_list <- num_sources() %>% map(function(x) 
        checkboxGroupInput(
          inputId = paste0("rm_checkbox_", x),
          label = paste0(sources_selected()[x,2], ":"),
          choiceNames = map(tables_per_source()[[x]][,1], function(x) group_name(x)),
          choiceValues = tables_per_source()[[x]][,1]
        )
      )
      do.call(tagList, checkbox_list)
    }
    
  })

  ######################
  ##RENDERUI FOR TABS
  #######################
  output$table_tabs <- renderUI({
    if(!(is.null(data_tbls()[[1]]))) {
      nTabs = nrow(rv$gruppi)
      myTabs = map(seq_len(nTabs), function(x)
        if(nrow(rv$gruppi) >= x) {
          tabPanel(
            title = c(group_n()[x], tipify(el = icon(name = "info-circle", 
                                                     lib = "font-awesome"), title = rv$tips[x],
                                           options = list(container="body"))),
            br(),
            if(include_moe()[x] == TRUE & no_pct_col()[x] == FALSE) {
              renderUI({
                tagList(
                  prettyCheckbox(inputId = paste0("pct_checkbox_", x), label = "Show % of total",
                                 value = TRUE, thick = TRUE,
                                 shape = "curve", status = "info", outline = TRUE, inline = TRUE),
                  prettyCheckbox(inputId = paste0("moe_checkbox_", x), label = "Show margin of error",
                                 value = FALSE, thick = TRUE,
                                 shape = "curve", status = "info", outline = TRUE, inline = TRUE)
                )
                
              })
            } else if(include_moe()[x] == FALSE & no_pct_col()[x] == FALSE) {
              renderUI({
                prettyCheckbox(inputId = paste0("pct_checkbox_", x), label = "Show % of total",
                               value = TRUE, thick = TRUE,
                               shape = "curve", status = "info", outline = TRUE, inline = TRUE)
              })
            } else if(include_moe()[x] == TRUE & no_pct_col()[x] == TRUE) {
              # prettyCheckbox(inputId = paste0("moe_checkbox_", x), label = "Show margin of error",
              #                value = FALSE, thick = TRUE,
              #                shape = "curve", status = "info", outline = TRUE, inline = TRUE)
            },
            br(),
            DT::dataTableOutput(outputId = paste0("table_", x)),
            br(), br(),
            HTML({"<b>Table Details</b>"}),
            #renderText(table_source1()),
            renderText(table_source3()[[x]]),
            renderText(table_source4())
          )
        }
      )
      do.call(tabsetPanel, c(myTabs, id = "dipset"))
    }
  })
  
  ###############################
  ###ACTION BUTTONS
  ###################################
  #clear tables if region is changed
  observeEvent(input$region, {
    rv$gruppi <- data.frame(group = character(), source = character(), year = character(), stringsAsFactors = FALSE)
  })
  #clear tables if areatype is changed
  observeEvent(input$areatype_selector, {
    rv$gruppi <- data.frame(group = character(), source = character(), year = character(), stringsAsFactors = FALSE)
  })
  #clear tables if area is changed
  observeEvent(input$area_selector, {
    rv$gruppi <- data.frame(group = character(), source = character(), year = character(), stringsAsFactors = FALSE)
  })
  #add to tables (rv$gruppi) from input$group
 observeEvent(input$group, {
   tempdf <- data.frame(group = groups_picked(), source = rep(datfileid(), length(groups_picked())),
                        year = rep(yearid(), length(groups_picked())), stringsAsFactors = FALSE)
   rv$gruppi <- rbind(rv$gruppi, tempdf) %>% unique()
 })
 #add tables to rv$rm_tables
 #observeEvent on all checkboxes to add selected tables to rm list
 observe(
   num_sources() %>% map(function(x)
     observeEvent(input[[paste0("rm_checkbox_", x)]], {
       rv$rm_tables <- c(rv$rm_tables, input[[paste0("rm_checkbox_", x)]]) %>% unique()
     })
     )
 )
 #clear tables from rv$gruppi
   observeEvent(input$btn_clear, {
    rv$gruppi <- rv$gruppi %>%
      filter(!(group %in% rv$rm_tables))
   })

  ###################################
  #####TABLES
  #####CREATE TABLES WITH RENDERTABLE
   observe(
     num_tables() %>% map(function(x)
       # when %change not available
       if(no_pct_col()[x] == TRUE) {
         area <- quote(area_column())
         # if(include_moe()[[x]] == TRUE) {data_use = data_tbls()[[x]][,c(1,2,4)]}
         # if(include_moe()[[x]] == FALSE) {data_use = data_tbls()[[x]][,1:2]}
         data_use = data_tbls()[[x]][,1:2]
         output[[paste0("table_", x)]] <- DT::renderDataTable({
           
           DT::datatable(data = data_use,
                         options = list(pageLength = 10,
                                        lengthMenu = list(c(10, 25, 100, -1),
                                                          c("10", "25", "100", "All"))),
                         rownames = FALSE) %>%
             DT::formatCurrency(eval(area), currency = "", digits = 0, interval = 3, mark = ",")
         })
         #when %change is available
       } else if(no_pct_col()[x] == FALSE) {
         area <- quote(area_column())
         if(include_moe()[[x]] == TRUE) {
           if(!is.na(rv$moe[x]) & !is.na(rv$pct[x])) {
             if(rv$moe[x] == TRUE & rv$pct[x] == TRUE) {data_use = data_tbls()[[x]]}
             if(rv$moe[x] == FALSE & rv$pct[x] == TRUE) {data_use = data_tbls()[[x]][,1:3] }
             if(rv$moe[x] == FALSE & rv$pct[x] == FALSE) {data_use = data_tbls()[[x]][,1:2] }
             if(rv$moe[x] == TRUE & rv$pct[x] == FALSE) {data_use = data_tbls()[[x]][,c(1,2,4)] }
           } else { data_use = data_tbls()[[x]] }
         }
         if(include_moe()[[x]] == FALSE) {
           if(!is.na(rv$pct[x])) {
             if(rv$pct[x] == TRUE) {data_use = data_tbls()[[x]][,1:3]}
             else {data_use = data_tbls()[[x]][,1:2]}
           }
         }
         if(!is.na(rv$pct[x])) {
           if(rv$pct[x] == TRUE) {
             output[[paste0("table_", x)]] <- DT::renderDataTable({
               DT::datatable(data = data_use,
                             options = list(pageLength = 10, #width = '80%',
                                            lengthMenu = list(c(10, 25, 100, -1),
                                                              c("10", "25", "100", "All"))),
                             rownames = FALSE) %>%
                 DT::formatPercentage('% of total', digits = 0) %>%
                 DT::formatCurrency(eval(area), currency = "", digits = 0, interval = 3, mark = ",") %>%
                 DT::formatStyle('% of total',
                                 background = DT::styleColorBar(range(c(0, data_tbls()[[x]][3]), na.rm = TRUE),
                                                                color = '#EFDA5B',
                                                                angle = -90),
                                 backgroundSize = '98% 88%',
                                 backgroundRepeat = 'no-repeat',
                                 backgroundPosition = 'center')
             })
           }
           else {
             output[[paste0("table_", x)]] <- DT::renderDataTable({
               
               DT::datatable(data = data_use,
                             options = list(pageLength = 10,
                                            lengthMenu = list(c(10, 25, 100, -1),
                                                              c("10", "25", "100", "All"))),
                             rownames = FALSE) %>%
                 DT::formatCurrency(eval(area), currency = "", digits = 0, interval = 3, mark = ",")
             })
           }
         }
       }
     )
   )

   
########################################
###DOWNLOAD BUTTON
  #############################

  output$download <- renderUI({
    if(!is.null(data_tbls()[[1]]) & length(rv$gruppi) > 1) {
      downloadButton(outputId = "download_data", label = "Download data")
    }
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste("infoshare", Sys.Date(), ".xls", sep ="")
    },
    content = function(file) {
      write.xlsx(data_tbls()[[1]], file, sheetName = str_replace_all(table_title_1()[1], "/", "."), 
                 row.names = FALSE)
      if(nrow(rv$gruppi) > 1) {
        map(2:nrow(rv$gruppi),function(x)
          write.xlsx(data_tbls()[[x]], file, sheetName = str_replace_all(table_title_1()[x], "/", "."), 
                     append = TRUE, row.names = FALSE)
        )
      }
      # if(length(rv$gruppi) > 1) {
      #   write.xlsx(data_tbls()[[2]], file, sheetName = table_title_1()[2], append = TRUE, row.names = FALSE)
      # }
      # write.xlsx(table_df(), file, sheetName = "table details", append = TRUE, row.names = FALSE, col.names = FALSE)
    
    }
  )
}

