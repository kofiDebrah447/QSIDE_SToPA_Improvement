
server <- (function(input, output, session){
  options(scipen = 4)
  options(shiny.maxRequestSize=30*1024^2)
  
  # Global variables ----
  globalVars <- reactiveValues()
  globalVars$sample <- FALSE
  globalVars$clean <- FALSE
  globalVars$changed <- TRUE
  
  updateUI <- function(changed){
    if(changed){
      shinyjs::hide("downloadresultsZip")
      hideTab(inputId = "main", target = "Q1")
      hideTab(inputId = "main", target = "Q2")
      hideTab(inputId = "main", target = "Q3")
      hideTab(inputId = "main", target = "Q4")
      hideTab(inputId = "main", target = "Q5")
      hideTab(inputId = "main", target = "Q6")
      hideTab(inputId = "main", target = "Q7")
      hideTab(inputId = "main", target = "Q8")
      hideTab(inputId = "main", target = "Q9")
      hideTab(inputId = "main", target = "Q10")
      hideTab(inputId = "main", target = "Q11")
      hideTab(inputId = "main", target = "Q12")
      hideTab(inputId = "main", target = "Q13")
      
      shinyjs::show("completeAnalysis")
    }else{
      shinyjs::show("downloadresultsZip")
      showTab(inputId = "main", target = "Q1")
      showTab(inputId = "main", target = "Q2")
      showTab(inputId = "main", target = "Q3")
      showTab(inputId = "main", target = "Q4")
      showTab(inputId = "main", target = "Q5")
      showTab(inputId = "main", target = "Q6")
      showTab(inputId = "main", target = "Q7")
      showTab(inputId = "main", target = "Q8")
      showTab(inputId = "main", target = "Q9")
      showTab(inputId = "main", target = "Q10")
      showTab(inputId = "main", target = "Q11") 
      showTab(inputId = "main", target = "Q12") 
      showTab(inputId = "main", target = "Q13")
      
      shinyjs::hide("completeAnalysis")
    }
  }
  
  
  updateUI(isolate(globalVars$changed))
  
  shinyjs::hide("county")
  shinyjs::hide("municipality")
  
  
  ##############################################
  # PROCESS UPLOADED DATA
  ##############################################
  # Process uploaded dataset ----
  upload_data <- reactive({
    req(input$file_upload)
    showModal(modalDialog("Loading Data...", footer=NULL))
    tryCatch({
      policingdata <- read_csv(input$file_upload$datapath)},
      error = function(e){
        stop(safeError(e))      # parsing error
      }
    )
    
    globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
    globalVars$dataset.original <- policingdata %>% mutate_if(is.character,as.factor)
    checkdataANDupdate()
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    removeModal()
  })  
  
  checkdataANDupdate <- function(){
    default.column.names <- c("Race", "Gender", "_SELECTED_CHARGE_1",
                              "Arrest", "BondAmount", "Patrol",
                              "Officer",  "DateTime", "TimeZone")
    
    if(all(default.column.names %in% colnames(globalVars$dataset))){
      # fill race column select
      
      updateSelectizeInput(session, "select_race_column", choices = c(colnames(globalVars$dataset)), selected = "Race")
      # fill race selects
      updateSelectizeInput(session, "select_aian", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("AIAN" %in% as.character(unique(globalVars$dataset[["Race"]])), "AIAN", "Not in the Data"))
      updateSelectizeInput(session, "select_asian", choices = c("","Not in the Data", as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("Asian" %in% as.character(unique(globalVars$dataset[["Race"]])), "Asian", "Not in the Data"))
      updateSelectizeInput(session, "select_black", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("Black" %in% as.character(unique(globalVars$dataset[["Race"]])), "Black", "Not in the Data"))
      updateSelectizeInput(session, "select_hispanic", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("Hispanic/Latino" %in% as.character(unique(globalVars$dataset[["Race"]])), "Hispanic/Latino", "Not in the Data"))
      updateSelectizeInput(session, "select_nhpi", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("NHPI" %in% as.character(unique(globalVars$dataset[["Race"]])), "NHPI", "Not in the Data"))
      updateSelectizeInput(session, "select_white", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("White" %in% as.character(unique(globalVars$dataset[["Race"]])), "White", "Not in the Data"))
      updateSelectizeInput(session, "select_multi", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("Multiracial" %in% as.character(unique(globalVars$dataset[["Race"]])), "Multiracial", "Not in the Data"))
      updateSelectizeInput(session, "select_notlisted", choices = c("","Not in the Data",as.character(unique(globalVars$dataset[["Race"]]))), selected = ifelse("A race not listed above" %in% as.character(unique(globalVars$dataset[["Race"]])), "A race not listed above", "Not in the Data"))
      
      # fill gender column select
      updateSelectizeInput(session, "select_gender_column", choices = c(colnames(globalVars$dataset)), selected = "Gender")
      
      # fill gender selects
      updateSelectizeInput(session, "select_man", choices = c("", "Not in the Data", as.character(unique(globalVars$dataset[["Gender"]]))), selected = "Man")
      updateSelectizeInput(session, "select_woman", choices = c("", "Not in the Data", as.character(unique(globalVars$dataset[["Gender"]]))), selected = "Woman")
      
      
      # fill charge column selectize
      charges <- grep(x = colnames(globalVars$dataset), pattern = "_SELECTED_CHARGE_", value = TRUE)
      updateSelectizeInput(session, "select_charges", choices = colnames(globalVars$dataset), selected = charges)
      
      # fill arrest column select
      updateSelectizeInput(session, "select_arrest", choices = c(colnames(globalVars$dataset)), selected = "Arrest")
      
      # fill arrest type selectize 
      updateSelectizeInput(session, "select_arrestTypes", choices = unique(globalVars$dataset[["Arrest"]]), selected = "TRUE")
      
      # fill bond amount column select
      updateSelectizeInput(session, "select_bond", choices = c(colnames(globalVars$dataset)), selected = "BondAmount")
      
      # fill patrol column select
      updateSelectizeInput(session, "select_patrol", choices = c(colnames(globalVars$dataset)), selected = "Patrol")
      
      # fill arresting officer column select
      updateSelectizeInput(session, "select_arrestingofficer", choices = c(colnames(globalVars$dataset)), selected = "Officer")
      
      # fill date column select
      updateSelectizeInput(session, "select_date", choices = c(colnames(globalVars$dataset)), selected = "DateTime")
      
      # fill timezone
      updateSelectizeInput(session, "select_timezone", 
                           choices = c("","US/Central", "US/Eastern", "US/Mountain", 
                                       "US/Pacific", "UTC"),
                           selected = unique(globalVars$dataset[["TimeZone"]]))
      # fill use of force column select
      updateSelectizeInput(session, "select_useofforce", choices = c("Not in the Data", colnames(globalVars$dataset)), selected = ifelse("UseOfForce" %in% colnames(globalVars$dataset), "UseOfForce", "Not in the Data"))
      
      # fill search conducted column select
      updateSelectizeInput(session, "select_searchconducted", choices = c("Not in the Data", colnames(globalVars$dataset)), selected = ifelse("SearchConducted" %in% colnames(globalVars$dataset), "SearchConducted", "Not in the Data"))
      
      # fill stopped column select
      updateSelectizeInput(session, "select_stopped", choices = c("Not in the Data", colnames(globalVars$dataset)), selected = ifelse("Stopped" %in% colnames(globalVars$dataset), "Stopped", "Not in the Data"))
      
      globalVars$clean <- TRUE
    }else{
      # update all column inputs to have column names to select
      # fill race column select
      updateSelectizeInput(session, "select_race_column", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill gender column select
      updateSelectizeInput(session, "select_gender_column", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill charge column selectize
      updateSelectizeInput(session, "select_charges", choices = colnames(globalVars$dataset), selected = "")
      
      # fill arrest column select
      updateSelectizeInput(session, "select_arrest", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill bond amount column select
      updateSelectizeInput(session, "select_bond", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill patrol column select
      updateSelectizeInput(session, "select_patrol", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill arresting officer column select
      updateSelectizeInput(session, "select_arrestingofficer", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill date column select
      updateSelectizeInput(session, "select_date", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill timezone
      updateSelectizeInput(session, "select_timezone", 
                           choices = c("", "US/Central", "US/Eastern", "US/Mountain", 
                                       "US/Pacific", "UTC"),
                           selected = )
      
      # fill use of force column select
      updateSelectizeInput(session, "select_useofforce", choices = c("Not in the Data", colnames(globalVars$dataset)), selected = "")
      
      # fill search conducted column select
      updateSelectizeInput(session, "select_searchconducted", choices = c("Not in the Data", colnames(globalVars$dataset)), selected = "")
      
      # fill stopped column select
      updateSelectizeInput(session, "select_stopped", choices = c("Not in the Data",colnames(globalVars$dataset)), selected = "")
      
      globalVars$clean <- FALSE
    }
  }
  
  ##############################################
  # DATASET PREVIEW
  ##############################################
  output$preview.data <- DT::renderDataTable({
    DT::datatable(globalVars$dataset)
  })
  
  ##############################################################################################################
  # Change UI based on user 
  ##############################################################################################################
  observe({
    if("" %in% c(input$select_race_column, input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted, 
                 input$select_gender_column, input$select_woman, input$select_man, 
                 input$select_charges, input$select_arrest, input$select_arrestTypes, input$select_bond, input$select_patrol, input$select_arrestingofficer, 
                 input$select_date, input$select_timezone, input$select_useofforce, input$select_searchconducted, input$selected_stopped,
                 input$census_api_key, input$census_year, input$geolevel, input$state, input$county, input$municipality)){
      shinyjs::enable("completeAnalysis")
    }else{
      shinyjs::enable("completeAnalysis")
    }
  })
  
  observeEvent(input$select_race_column,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # fill race selects
    race.choices <- c("", "Not in the Data", as.character(unique(globalVars$dataset[[input$select_race_column]])))
    updateSelectizeInput(session, "select_aian", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_asian", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_black", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_hispanic", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_nhpi", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_white", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_multi", choices = race.choices, selected = "")
    updateSelectizeInput(session, "select_notlisted", choices = race.choices, selected = "")
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_race_column))
    # fill race column select
    #updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_aian,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])),
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    # fill race selects
    # updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  
  observeEvent(input$select_asian,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    #updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_black,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    #updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_hispanic,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    #updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_nhpi,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    #updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_white,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    #updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_multi,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    #updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  
  observeEvent(input$select_notlisted,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_race_column]])), 
                                                    c(input$select_aian, input$select_asian, input$select_black, input$select_hispanic, input$select_nhpi, input$select_white, input$select_multi, input$select_notlisted)))
    
    # fill race selects
    updateSelectizeInput(session, "select_aian", choices = c(input$select_aian, otherselects), selected = input$select_aian)
    updateSelectizeInput(session, "select_asian", choices = c(input$select_asian, otherselects), selected = input$select_asian)
    updateSelectizeInput(session, "select_black", choices = c(input$select_black, otherselects), selected = input$select_black)
    updateSelectizeInput(session, "select_hispanic", choices = c(input$select_hispanic, otherselects), selected = input$select_hispanic)
    updateSelectizeInput(session, "select_nhpi", choices = c(input$select_nhpi, otherselects), selected = input$select_nhpi)
    updateSelectizeInput(session, "select_white", choices = c(input$select_white, otherselects), selected = input$select_white)
    updateSelectizeInput(session, "select_multi", choices = c(input$select_multi, otherselects), selected = input$select_multi)
    #updateSelectizeInput(session, "select_notlisted", choices = c(input$select_notlisted, otherselects), selected = input$select_notlisted)
  })
  
  observeEvent(input$select_gender_column,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # fill gender selects
    updateSelectizeInput(session, "select_man", choices = c("","Not in the Data", as.character(unique(globalVars$dataset[[input$select_gender_column]]))), selected = input$select_man)
    updateSelectizeInput(session, "select_woman", choices = c("","Not in the Data", as.character(unique(globalVars$dataset[[input$select_gender_column]]))), selected = input$select_man)
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_gender_column))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    #updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce) 
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_woman,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_gender_column]])),input$select_woman))
    
    # fill race selects
    #updateSelectizeInput(session, "select_woman", choices = otherselects)
    updateSelectizeInput(session, "select_man", choices = otherselects, selected=input$select_man)
  })
  
  observeEvent(input$select_man,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("","Not in the Data", setdiff(as.character(unique(globalVars$dataset[[input$select_gender_column]])),input$select_man))
    
    # fill race selects
    updateSelectizeInput(session, "select_woman", choices = otherselects, selected = input$select_woman)
    #updateSelectizeInput(session, "select_man", choices = otherselects)
  })
  
  observeEvent(input$select_charges,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_charges))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    #updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_arrest,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # fill arrest type selectize 
    
    
    if(globalVars$clean){
      updateSelectizeInput(session, "select_arrestTypes", choices = unique(globalVars$dataset[[input$select_arrest]]), selected="TRUE")
    }else{
      updateSelectizeInput(session, "select_arrestTypes", choices = unique(globalVars$dataset[[input$select_arrest]]))
    }
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_charges))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    #updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_arrestTypes,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  observeEvent(input$select_bond,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_bond))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    #updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped) 
  })
  
  observeEvent(input$select_patrol,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_patrol))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    #updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_arrestingofficer,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_arrestingofficer))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    #updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_date,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_date))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    #updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce)
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted)
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped)
  })
  
  observeEvent(input$select_timezone,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  observeEvent(input$census_api_key,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  observeEvent(input$census_year,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  observeEvent(input$geolevel,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  
  observeEvent(input$state,{
    if(input$state != ""){
      tryCatch(
        expr = {
          census_api_key(input$census_api_key)
          counties <- str_split(get_acs(geography = "county", variables = "B01001_001E", state=input$state)$NAME, pattern = " County", simplify = T)[,1]
          updateSelectizeInput(session, inputId = "county", choices = c(counties), selected = "")
          shinyjs::show("county")
          shinyjs::hide("municipality")
          globalVars$changed <- TRUE
          updateUI(isolate(globalVars$changed))
        },
        error=function(e){
          shinyalert("Oops!", "Something went wrong. Check your census api key.", type = "error")
          updateSelectizeInput(session, inputId = "county", 
                               choices = c("","AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                                           "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                                           "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                           "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                                           "NY","NC", "ND", "OH", "OK", "OR", "PA",
                                           "PR", "RI", "SC", "SD", "TN", "TX", "UT",
                                           "VT", "VA", "VI", "WA", "WV", "WI", "WY"),
                               selected = "",
                               options = list(placeholder = "Select a State"))
        }
      )
    }else{
      shinyjs::hide("county")
      shinyjs::hide("municipality")
    }
  })
  
  observeEvent(input$county, {
    if(input$county != ""){
      municipalities <- read.csv("www/placeList.txt", sep="|") %>%
        filter(STATE == input$state) %>%
        filter(grepl(pattern=paste(input$county, "County"), x=COUNTY)) %>%
        pull(PLACENAME) %>%
        gsub(pattern="\\s*\\w*$", replacement="")
      
      updateSelectizeInput(session, inputId = "municipality", choices = c(municipalities), selected = "")
      
      globalVars$changed <- TRUE
      updateUI(isolate(globalVars$changed))
      shinyjs::show("municipality")
    }
  })
  
  observeEvent(input$municipality,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
  })
  
  observeEvent(input$select_useofforce,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_useofforce))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    #updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce) ###### ADDED
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted) ###### ADDED
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped) ###### ADDED
  })
  
  ##########
  
  
  observeEvent(input$select_searchconducted,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_searchconducted))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce) ###### ADDED
    # fill search conducted column select
    #updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted) ###### ADDED
    # fill stopped column select
    updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped) ###### ADDED
  })
  
  #############
  
  observeEvent(input$select_stopped,{
    globalVars$changed <- TRUE
    updateUI(isolate(globalVars$changed))
    
    # update all column inputs to have column names to select
    otherselects <- c("Not in the Data", setdiff(colnames(globalVars$dataset), input$select_stopped))
    # fill race column select
    updateSelectizeInput(session, "select_race_column", choices = c(input$select_race_column, otherselects), selected=input$select_race_column)
    # fill gender column select
    updateSelectizeInput(session, "select_gender_column", choices = c(input$select_gender_column, otherselects), selected=input$select_gender_column)
    # fill charge column selectize
    updateSelectizeInput(session, "select_charges", choices = c(input$select_charges, otherselects), selected=input$select_charges)
    # fill arrest column select
    updateSelectizeInput(session, "select_arrest", choices = c(input$select_arrest, otherselects), selected=input$select_arrest)
    # fill bond amount column select
    updateSelectizeInput(session, "select_bond", choices = c(input$select_bond, otherselects), selected=input$select_bond)
    # fill patrol column select
    updateSelectizeInput(session, "select_patrol", choices = c(input$select_patrol, otherselects), selected=input$select_patrol)
    # fill arresting officer column select
    updateSelectizeInput(session, "select_arrestingofficer", choices = c(input$select_arrestingofficer, otherselects), selected=input$select_arrestingofficer)
    # fill date column select
    updateSelectizeInput(session, "select_date", choices = c(input$select_date, otherselects), selected=input$select_date)
    # fill use of force column select
    updateSelectizeInput(session, "select_useofforce", choices = c(input$select_useofforce, otherselects), selected=input$select_useofforce) ###### ADDED
    # fill search conducted column select
    updateSelectizeInput(session, "select_searchconducted", choices = c(input$select_searchconducted, otherselects), selected=input$select_searchconducted) ###### ADDED
    # fill stopped column select
    #updateSelectizeInput(session, "select_stopped", choices = c(input$select_stopped, otherselects), selected=input$select_stopped) ###### ADDED
  })
  
  
  ##############################################################################################################
  # Upload Data
  ##############################################################################################################
  observeEvent(input$file_upload,{
    inFile <<- upload_data()
  })
  
  ##############################################################################################################
  # Load Sample Data
  ##############################################################################################################
  observeEvent(input$sample, {
    if(!globalVars$sample){
      globalVars$sample <- TRUE
      shinyjs::hide("file")
      shinyjs::hide("file_upload")
      shinyjs::show("choose_sample")
      
      if(input$sample_data_choice=="Durham NC"){
        policingdata <- read_csv("www/durhamdata.csv")
      }
      
      globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
      globalVars$dataset.original <- policingdata 
      checkdataANDupdate()
      
      globalVars$changed <- TRUE
      updateUI(isolate(globalVars$changed))
      
      updateActionButton(session, "sample", label = "<- Back")
    } else {
      globalVars$sample <- FALSE
      globalVars$dataset <- NULL
      shinyjs::show("file")
      shinyjs::show("file_upload")
      shinyjs::hide("choose_sample")
      
      updateActionButton(session, "sample", label = "Sample dataset")
      
      # update all column inputs to have column names to select
      # fill race column select
      updateSelectizeInput(session, "select_race_column", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill gender column select
      updateSelectizeInput(session, "select_gender_column", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill charge column selectize
      updateSelectizeInput(session, "select_charges", choices = colnames(globalVars$dataset), selected = "")
      
      # fill arrest column select
      updateSelectizeInput(session, "select_arrest", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill bond amount column select
      updateSelectizeInput(session, "select_bond", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill patrol column select
      updateSelectizeInput(session, "select_patrol", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill arresting officer column select
      updateSelectizeInput(session, "select_arrestingofficer", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill date column select
      updateSelectizeInput(session, "select_date", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill timezone
      updateSelectizeInput(session, "select_timezone", 
                           choices = c("", "US/Central", "US/Eastern", "US/Mountain", 
                                       "US/Pacific", "UTC"),
                           selected = )
      
      updateSelectizeInput(session, "state",
                           choices = c("", "AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                                       "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                                       "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                       "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                                       "NY","NC", "ND", "OH", "OK", "OR", "PA",
                                       "PR", "RI", "SC", "SD", "TN", "TX", "UT",
                                       "VT", "VA", "VI", "WA", "WV", "WI", "WY"),
                           selected="")
      updateSelectizeInput(session, "county", choices=c(""), selected="")
      updateSelectizeInput(session, "municipality", choices=c(""), selected="")
      
      # fill use of force column select
      updateSelectizeInput(session, "select_useofforce", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill search conducted column select
      updateSelectizeInput(session, "select_searchconducted", choices = c(colnames(globalVars$dataset)), selected = "")
      
      # fill stopped column select
      updateSelectizeInput(session, "select_stopped", choices = c(colnames(globalVars$dataset)), selected = "")
      
      globalVars$clean <- FALSE
      
      globalVars$dataset <- NULL
      globalVars$dataset.original <- NULL
    }
  })
  
  
  observeEvent(input$sample_data_choice,{
    if(globalVars$sample){
      if(input$sample_data_choice=="Durham NC"){
        policingdata <- read_csv("www/durhamdata.csv")
      }
      
      globalVars$dataset <- policingdata %>% mutate_if(is.character,as.factor)
      globalVars$dataset.original <- policingdata %>% mutate_if(is.character,as.factor)      
      checkdataANDupdate()
      globalVars$changed <- TRUE
      updateUI(isolate(globalVars$changed))
    }
  })
  
  
  observeEvent(input$completeAnalysis, {
    if(globalVars$clean){
      analyzeData() 
    }else{
      cleanData()
      analyzeData()
    }
  })
  
  cleanData <- function(){
    showModal(modalDialog("Cleaning the Data!", footer=NULL))
    
    policingdata <- globalVars$dataset
    
    
    # Race
    racecolumn <- ifelse(input$select_race_column=="Not in the Data", NA, input$select_race_column)
    americanindianalaskanativecode <- ifelse(input$select_aian=="Not in the Data", NA, input$select_aian)
    asiancode <- ifelse(input$select_asian=="Not in the Data", NA, input$select_asian)
    blackcode <- ifelse(input$select_black=="Not in the Data", NA, input$select_black)
    hispaniclatinocode <- ifelse(input$select_hispanic=="Not in the Data", NA, input$select_hispanic)
    nativehawaiianpacificislandercode <- ifelse(input$select_nhpi=="Not in the Data" , NA, input$select_nhpi)
    whitecode <- ifelse(input$select_white=="Not in the Data", NA, input$select_white)
    multiracialcode <- ifelse(input$select_multi=="Not in the Data", NA, input$select_multi)
    notlistedcode <- ifelse(input$select_notlisted=="Not in the Data", NA, input$select_notlisted)
    
    # Gender
    gendercolumn <- input$select_gender_column
    womancode <- input$select_woman
    mancode <- input$select_man
    
    # Charge
    chargecolumns <- input$select_charges
    
    # Type of arrest
    typeofarrestcolumn <- input$select_arrest
    actualarresttypes <- input$select_arrestTypes
    
    # Bond amount column
    bondamountcolumn <- input$select_bond
    
    # Patrolling geo unit
    patrolcolumn <- input$select_patrol
    
    # Officer
    officercolumn <- input$select_arrestingofficer
    
    # Date and time
    datetimecolumn <- input$select_date
    timezone <- input$select_timezone
    
    # Use of Force
    useofforcecolumn <- ifelse(input$select_useofforce=="Not in the Data", NA, input$select_useofforce) 
    
    # Search Conducted
    searchconductedcolumn <- ifelse(input$select_searchconducted=="Not in the Data", NA, input$select_searchconducted)
    
    # Stopped
    stoppedcolumn <- ifelse(input$select_stopped=="Not in the Data", NA, input$select_stopped)
    
    
    #################################
    ### Tidy up policing data set ###
    #################################
    # Set columns to keep
    keeps <- c(racecolumn, gendercolumn, chargecolumns, typeofarrestcolumn , bondamountcolumn, patrolcolumn, officercolumn, datetimecolumn)
    
    # Load data
    policingdata <- policingdata %>%
      select(all_of(keeps))
    
    # Recode race data
    policingdata <- policingdata %>%
      rename(Race = !!racecolumn) %>%
      mutate(Race = dplyr::case_match(Race,
                                      americanindianalaskanativecode ~ "AIAN",
                                      asiancode ~ "Asian",
                                      blackcode ~ "Black",
                                      hispaniclatinocode ~ "Hispanic/Latino",
                                      nativehawaiianpacificislandercode ~ "NHPI",
                                      whitecode ~ "White",
                                      multiracialcode ~ "Multiracial",
                                      notlistedcode ~ "A race not listed above",
                                      .default = "Missing race data")) %>%
      mutate(Race = factor(Race, levels = c("AIAN",
                                            "Asian",
                                            "Black",
                                            "Hispanic/Latino",
                                            "NHPI",
                                            "White",
                                            "Multiracial",
                                            "A race not listed above",
                                            "Missing race data")))
    
    # Recode gender data
    policingdata <- policingdata %>%
      rename(Gender = !!gendercolumn) %>%
      mutate(Gender = case_match(Gender,
                                 mancode ~ "Man",
                                 womancode ~ "Woman",
                                 .default = "Missing gender data")) %>%
      mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data")))
    
    # Rename charges columns
    charge.columns.index <- which(colnames(policingdata)%in%input$select_charges)
    charge.columns.n <- length(charge.columns.index)
    colnames(policingdata)[charge.columns.index] <- paste("_SELECTED_CHARGE_", 1:charge.columns.n, sep="")
    
    # Type of arrest
    policingdata <- policingdata %>%
      rename(typeofarrest = !!typeofarrestcolumn) %>%
      mutate(typeofarrest = str_to_title(typeofarrest)) %>%
      mutate(typeofarrest = replace(typeofarrest, is.na(typeofarrest), "Missing arrest type data")) %>%
      mutate(typeofarrest = factor(typeofarrest, levels = c(unique(typeofarrest), "Missing arrest type data")))
    
    # Type of arrest
    policingdata <- policingdata %>%
      mutate(Arrest = typeofarrest %in% str_to_title(input$select_arrestTypes)) %>%
      select(-typeofarrest)
    
    # Bond amount
    policingdata <- policingdata %>%
      rename(BondAmount = !!bondamountcolumn) %>%
      mutate(BondAmount = str_replace_all(BondAmount, "(\\$|\\,)", "")) %>%
      mutate(BondAmount = as.numeric(BondAmount))
    
    # Organize patrol geo units
    policingdata <- policingdata %>%
      rename(Patrol = !!patrolcolumn) %>%
      mutate(Patrol = factor(Patrol))
    
    # Organize officers
    policingdata <- policingdata %>%
      rename(Officer = !!officercolumn) %>%
      mutate(Officer = factor(Officer))
    
    # Rename Dates
    policingdata <- policingdata %>%
      rename(DateTime = !!datetimecolumn) %>%
      mutate(TimeZone = input$select_timezone)
    
    # Use of Force
    policingdata <- policingdata %>%
      rename(UseOfForce = !!useofforcecolumn) %>% 
      mutate(UseOfForce = factor(UseOfForce))
    
    # Search Conducted
    policingdata <- policingdata %>%
      rename(SearchConducted = !!searchconductedcolumn) %>% 
      mutate(SearchConducted = factor(SearchConducted))
    
    # Stopped
    policingdata <- policingdata %>%
      rename(Stopped = !!stoppedcolumn) %>%
      mutate(Stopped = factor(Stopped))
    
    # Put in nice order
    policingdata <- policingdata %>%
      relocate(Race, Gender, Arrest, BondAmount, Patrol, Officer, DateTime, UseOfForce, SearchConducted, Stopped, starts_with("_SELECTED_CHARGE_")) %>%
      as.data.frame %>%
      remove_attributes("spec")
    
    globalVars$dataset <- policingdata
    
    removeModal()
  }
  
  analyzeData <- function(){
    tryCatch(
      expr = {
        showModal(modalDialog("Pulling Census Data", footer=NULL))
        ##################
        ### User input ###
        ##################
        censusapikey <- input$census_api_key
        acsyear <- as.numeric(input$census_year)
        geolevel <- input$geolevel
        # Note: race x gender not available below tract level
        #       once up and running we can test different geographies
        
        # Geographic info
        municipality <- input$municipality
        county <- input$county
        state <- input$state
        
        ##########################
        ### Set Census API key ###
        ##########################
        census_api_key(censusapikey)
        
        ########################################
        ### Define relevant census variables ###
        ########################################
        acs5vars <- load_variables(acsyear, "acs5")
        # See https://www.census.gov/newsroom/blogs/random-samplings/2021/08/measuring-racial-ethnic-diversity-2020-census.html
        
        totalpop_var <- "B01001_001"
        
        racegender_vars <- acs5vars %>%
          filter(geography == geolevel) %>%
          filter(str_detect(concept, "^SEX BY AGE \\(.*\\)$")) %>%
          filter(!str_detect(concept, "\\(WHITE ALONE\\)")) %>%
          filter(str_detect(label, "(Male|Female):$")) %>%
          select(-geography)
        
        income_vars <- acs5vars %>%
          filter(geography == geolevel) %>%
          filter(str_detect(concept, "^HOUSEHOLD INCOME IN THE PAST 12 MONTHS.*HOUSEHOLDER\\)$")) %>%
          filter(!str_detect(concept, "\\(WHITE ALONE HOUSEHOLDER\\)$")) %>%
          filter(label != "Estimate!!Total:") %>%
          select(-geography)
        
        ###############################
        ### Get map of municipality ###
        ###############################
        municipalitysf <- places(state = state, year = acsyear, cb = TRUE) %>%
          filter(NAME == municipality)
        
        ###################################################
        ### Get county census data including geometries ###
        ###################################################
        acsracegender <- get_acs(geography = geolevel,
                                 variables = racegender_vars$name,
                                 year = acsyear,
                                 state = state,
                                 county = county,
                                 geometry = TRUE)
        
        acsincome <- get_acs(geography = geolevel,
                             variables = income_vars$name,
                             year =  acsyear,
                             state = state,
                             county = county,
                             geometry = TRUE)
        
        ############################################
        ### Restrict county data to municipality ###
        ############################################
        # Note:
        # A consequential choice we make is to keep all census tracts in county that
        # touch the municipality. An alternative approach would be to intersect
        # the tracts with the municipality and weight ACS data accordingly. However,
        # this is tricky as it assumes spatially uniform distributions and ignores
        # water features.
        
        acsracegender <- st_join(municipalitysf, acsracegender, join = st_intersects) %>%
          select(variable, estimate, geometry) %>%
          st_set_geometry(NULL) %>%
          merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name")
        
        acsincome <- st_join(municipalitysf, acsincome, join = st_intersects) %>%
          select(variable, estimate, geometry) %>%
          st_set_geometry(NULL) %>%
          merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name")
        
        #########################################
        ### Calculate municipality-wide stats ###
        #########################################
        
        # Calculate the race/gender stats and recode
        acsracegender <- acsracegender %>%
          group_by(variable) %>%
          summarise(estimate = sum(estimate)) %>%
          merge(racegender_vars, all.x = TRUE, by.x = "variable", by.y = "name") %>%
          select(-variable) %>%
          rename(people = estimate) %>%
          mutate(Gender = case_when(
            str_detect(label, "Male") ~ "Man",
            str_detect(label, "Female") ~ "Woman")) %>%
          mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data"))) %>%
          mutate(Race = case_when(
            str_detect(concept, "INDIAN") ~ "AIAN",
            str_detect(concept, "ASIAN") ~ "Asian",
            str_detect(concept, "BLACK") ~ "Black",
            str_detect(concept, "\\(HISPANIC OR LATINO\\)") ~ "Hispanic/Latino",
            str_detect(concept, "HAWAIIAN") ~ "NHPI",
            str_detect(concept, "WHITE") ~ "White",
            str_detect(concept, "MORE") ~ "Multiracial",
            str_detect(concept, "OTHER") ~ "A race not listed above")) %>%
          mutate(Race = factor(Race, levels = c("AIAN",
                                                "Asian",
                                                "Black",
                                                "Hispanic/Latino",
                                                "NHPI",
                                                "White",
                                                "Multiracial",
                                                "A race not listed above",
                                                "Missing race data"))) %>%
          mutate(proportion = prop.table(people)) %>%
          mutate(datatype = "Local Population") %>%
          select(Race, Gender, proportion, datatype)
        
        # Calculate the income stats and recode
        acsincome <- acsincome %>%
          group_by(variable) %>%
          summarise(estimate = sum(estimate)) %>%
          merge(income_vars, all.x = TRUE, by.x = "variable", by.y = "name") %>%
          select(-variable) %>%
          group_by(label) %>%
          summarise(households = sum(estimate)) %>%
          rename(income = label) %>%
          mutate(income = str_replace_all(income, "Estimate!!Total:!!", "")) %>%
          mutate(proportion = prop.table(households)) %>%
          mutate(datatype = "Local Population") %>%
          select(income, proportion, datatype)
        
        #################################
        ### Tidy up policing data set ###
        #################################
        policingdata <- globalVars$dataset
        
        policingdata <- policingdata %>%
          # Fix Race
          mutate(Race = case_when(is.na(Race) ~ NA_character_,
                                  TRUE        ~ Race)) %>%
          mutate(Race=factor(Race, levels = c("AIAN",
                                              "Asian",
                                              "Black",
                                              "Hispanic/Latino",
                                              "NHPI",
                                              "White",
                                              "Multiracial",
                                              "A race not listed above",
                                              "Missing race data"))) %>%
          # Fix Gender
          mutate(Gender = factor(Gender, levels = c("Man", "Woman", "Missing gender data")))
        
        # Make list of all possible charges
        chargelist <- policingdata %>%
          select(starts_with("_SELECTED_CHARGE_"))%>%
          #select(input$select_charges) %>%
          unlist %>%
          unname %>%
          unique %>%
          sort %>%
          data.frame(charge = .)
        
        # Identify routine traffic/moving violations
        # May eventually need user input to identify relevant charges
        trafficwords <- c("drive", "driving", "flashing", "headlamp", "headlight", "highway", "intersection", "lane", "license", "parking", "passing", "plate", "right of way", "speed", "stoplight", "stop light", "stopsign", "stop sign", "traffic", "vehicle", "windshield", "yield") %>%
          paste0(collapse = "|")
        nottrafficwords <- c("aggravated", "consuming", "death", "felony", "impaired", "fraud", "intoxicated", "stolen", "trafficking") %>%
          paste0(collapse = "|")
        trafficcharges <- chargelist %>%
          filter(str_detect(charge, regex(trafficwords, ignore_case = TRUE))) %>%
          filter(!str_detect(charge, regex(nottrafficwords, ignore_case = TRUE))) %>%
          pull(charge)%>%
          paste0(collapse = "|")
        tmp <- policingdata %>%
          select(starts_with("_SELECTED_CHARGE_"))
        #select(all_of(input$select_charges))
        tmp[] <- tmp %>%
          lapply(function(x) str_detect(x, regex(trafficcharges, ignore_case = TRUE)))
        policingdata <- policingdata %>%
          mutate(traffic = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
        # Identify firearms/drug possession
        # May eventually need user input to identify relevant charges
        druggunwords <- c("firearm", "possess control", "possess cs", "substance", "weapon") %>%
          paste0(collapse = "|")
        notdruggunwords <- c("alcohol", "assault", "burglar", "conspiracy", "counterfeit", "dischar", "dwelling", "embezzle", "fraudulent", "insanity", "larceny", "manufactur", "mass dest", "protect minors", "robbery", "sale", "sell", "trafficking") %>%
          paste0(collapse = "|")
        drugguncharges <- chargelist %>%
          filter(str_detect(charge, regex(druggunwords, ignore_case = TRUE))) %>%
          filter(!str_detect(charge, regex(notdruggunwords, ignore_case = TRUE))) %>%
          pull(charge) %>%
          paste0(collapse = "|")
        tmp <- policingdata %>%
          select(starts_with("_SELECTED_CHARGE_"))
        #select(all_of(input$select_charges))
        tmp[] <- tmp %>%
          lapply(function(x) str_detect(x, regex(drugguncharges, ignore_case = TRUE)))
        policingdata <- policingdata %>%
          mutate(druggun = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
        
        # Parse date and time
        policingdata <- policingdata %>%
          mutate(DateTime = mdy_hm(DateTime, tz = input$select_timezone)) %>%
          mutate(Date = date(DateTime), Day = wday(DateTime, label = TRUE), Time = as_hms(DateTime)) %>%
          select(-DateTime)
        
        # Identify quality of life crimes
        # May eventually need user input to identify relevant charges
        qolwords <- c("public", "disorderly", "loiter", "noise", "urinat", "vandal") %>%
          paste0(collapse = "|")
        notqolwords <- c("assault", "embezzle", "imperson", "controlled", " cs ", "resist") %>%
          paste0(collapse = "|")
        qolcharges <- chargelist %>%
          filter(str_detect(charge, regex(qolwords, ignore_case = TRUE))) %>%
          filter(!str_detect(charge, regex(notqolwords, ignore_case = TRUE))) %>%
          pull(charge) %>%
          paste0(collapse = "|")
        tmp <- policingdata %>%
          select(starts_with("_SELECTED_CHARGE_"))
        #select(all_of(input$select_charges))
        tmp[] <- tmp %>%
          lapply(function(x) str_detect(x, regex(qolcharges, ignore_case = TRUE)))
        policingdata <- policingdata %>%
          mutate(qol = apply(tmp, 1, function(x) any(x, na.rm = TRUE)))
        
        # Dump raw charges
        policingdata <- policingdata %>%
          select(-starts_with("_SELECTED_CHARGE_"))
        #select(-input$select_charges) 
        
        # Define the columns you wish to relocate, in order
        desired_order <- c("Race", "Gender", "traffic", "Arrest", "BondAmount", "druggun",
                           "Patrol", "Officer", "Date", "Day", "Time", "qol", "UseOfForce",
                           "SearchConducted", "Stopped")
        
        # Find which of those columns actually exist in your data frame
        existing_columns <- desired_order[desired_order %in% names(policingdata)]
        
        # Use relocate() with only the existing columns
        policingdata <- policingdata %>%
          relocate(all_of(existing_columns)) %>%
          as.data.frame() %>%
          remove_attributes("spec")
        
        
        ###########################
        ### Set up Excel output ###
        ###########################
        numq <- 13
        wb <- createWorkbook()
        for (i in 1:13) {
          addWorksheet(wb, paste0("Q",i))
        }
        centered <- createStyle(halign = "center")
        centeredrounded2 <- createStyle(halign = "center", numFmt = "0.00")
        centeredrounded3 <- createStyle(halign = "center", numFmt = "0.000")
        
        ### Below, question numbers correspond to questions as listed at:
        ### https://docs.google.com/spreadsheets/d/1A9x00WhdQccfwa3XolplvPtjBYgd32k-qEpDpFVfddo/edit#gid=0
        
        analyze_answerable_questions <- function(policingdata) {
          
          # Define required columns for each question
          question_cols <- list(
            Q1 = c("Race", "Gender"),
            Q2 = c("Race", "Gender", "traffic"),
            Q3 = c("Race", "Gender", "druggun"),
            Q4 = c("Race", "Gender", "qol"),
            Q5 = c("Race", "Gender", "Arrest"),
            Q6 = c("Race", "Gender", "Arrest"),
            Q7 = c("Race", "Gender", "BondAmount"),
            Q8 = c("Patrol", "Race"),
            Q9 = c("Officer", "Race"),
            Q10 = c("Day", "Time"),
            Q11 = c("Race", "Gender", "UseOfForce"),
            Q12 = c("Race", "Gender", "Stopped"),
            Q13 = c("Race", "Gender", "SearchConducted")
          )
          
          # Initialize counter for answerable questions
          answerable_questions <- 0
          
          # Loop through questions and check data availability
          for (question in names(question_cols)) {
            if (all(question_cols[[question]] %in% colnames(policingdata))) {
              answerable_questions <- answerable_questions + 1
            }
          }
          
          return(answerable_questions)
        }
        
        #answerable_count <- analyze_answerable_questions(policingdata)
        #cat("Number of questions answered are ", answerable_count)
        
        
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 1 of 13...", footer=NULL))
        question <- 1
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender") %in% colnames(policingdata)) & all(c("Race", "Gender") %in% colnames(acsracegender))) {
          
          # Data available, proceed with analysis
          observed <- policingdata %>%
            group_by(Race, Gender) %>%
            summarise(people = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(people = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          expected <- acsracegender %>%
            complete(Race, Gender, fill = list(proportion = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
          
          qdata <- observed %>%
            mutate(proportion = prop.table(people)) %>%
            select(-people) %>%
            mutate(datatype = "Policing Records") %>%
            rbind(acsracegender) %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records")) %>%
            filter(Gender != "Missing gender data") %>%
            filter(Race != "Missing race data")
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) +
            geom_bar(stat = "identity", position = "stack") +
            xlab("Reported Race") +
            ylab("Proportion") + 
            scale_fill_manual(values = c("coral", "blue"),
                              guide = guide_legend(title = "Reported Gender"), 
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.4, 0.8),
                               guide = guide_legend(title = "Data Type"),
                               labels = c("Policing Records", "Local Population")) +
            facet_wrap(~Gender)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          globalVars$p1 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
          
          globalVars$t1 <- qExcel
          
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          deleteData(wb, sheet = question, cols = 1, rows = 2)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
          writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
          #writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
          mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
          #mergeCells(wb, sheet = question, rows = 1, cols = 6:7) 
          writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
          writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
          #writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population") 
          writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
          writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
          #writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing")
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
          if (chisq$p.value < 0.05) {
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
            globalVars$m1 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }else{
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions do not appear to differ significantly from the population proportions."
            globalVars$m1 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q1_interp <- renderUI({
            text <- "Data for question Q1 is missing."
            return(tags$p(text))
          })
          globalVars$t1 <- NULL
        }
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 2 of 13...", footer=NULL))
        question <- 2
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "traffic") %in% colnames(policingdata))) {
          
          # Racial breakdown of traffic-related offenses
          observed <- policingdata %>%
            filter(traffic == TRUE) %>%
            group_by(Race, Gender) %>%
            summarise(people = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(people = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          expected <- acsracegender %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
          
          qdata <- observed %>%
            mutate(proportion = prop.table(people)) %>%
            select(-people) %>%
            mutate(datatype = "Policing Records") %>%
            rbind(acsracegender) %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records")) %>%
            filter(Gender != "Missing gender data") %>%
            filter(Race != "Missing race data")
          
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) +
            geom_bar(stat = "identity", position = "stack") +
            xlab("Reported Race") +
            ylab("Proportion") +
            scale_fill_manual(values = c("coral", "blue"),
                              guide = guide_legend(title = "Reported Gender"), 
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.3, 0.7), 
                               guide = guide_legend(title = "Data Type"), 
                               labels = c("Policing Records", "Local Population")) +
            facet_wrap(~Gender)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          
          globalVars$p2 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
          
          globalVars$t2 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          deleteData(wb, sheet = question, cols = 1, rows = 2)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
          writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
          #writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
          mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
          #mergeCells(wb, sheet = question, rows = 1, cols = 6:7) 
          writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
          writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
          #writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population") 
          writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
          writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
          # writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing") 
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
          if (chisq$p.value < 0.05) {
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
            globalVars$m2 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }else{
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions do not appear to differ significantly from the population proportions."
            globalVars$m2 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q2_interp <- renderUI({
            text <- "Data for question Q2 is missing."
            return(tags$p(text))
          })
          globalVars$t2 <- NULL
        }
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 3 of 13...", footer=NULL))
        question <- 3
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "druggun") %in% colnames(policingdata))) {
          
          observed <- policingdata %>%
            filter(druggun == TRUE) %>%
            group_by(Race, Gender) %>%
            summarise(people = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(people = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          expected <- acsracegender %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
          
          qdata <- observed %>%
            mutate(proportion = prop.table(people)) %>%
            select(-people) %>%
            mutate(datatype = "Policing Records") %>%
            rbind(acsracegender) %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records")) %>%
            filter(Gender != "Missing gender data") %>% 
            filter(Race != "Missing race data")
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) + 
            geom_bar(stat = "identity", position = "stack") +
            xlab("Reported Race") + 
            ylab("Proportion") + 
            scale_fill_manual(values = c("coral", "blue"),
                              guide = guide_legend(title = "Reported Gender"),
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.3, 0.7),
                               guide = guide_legend(title = "Data Type"), 
                               labels = c("Policing Records", "Local Population")) +
            facet_wrap(~Gender)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          globalVars$p3 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
          
          globalVars$t3 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          deleteData(wb, sheet = question, cols = 1, rows = 2)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
          writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
          #writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data") 
          mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
          mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
          #mergeCells(wb, sheet = question, rows = 1, cols = 6:7) 
          writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
          writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
          #writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population") 
          writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
          writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
          #writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing") 
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
          if (chisq$p.value < 0.05) {
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
            globalVars$m3 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }else{
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions do not appear to differ significantly from the population proportions."
            globalVars$m3 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q3_interp <- renderUI({
            text <- "Data for question Q3 is missing."
            return(tags$p(text))
          })
          globalVars$t3 <- NULL
        }
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 4 of 13...", footer=NULL))
        question <- 4
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "qol") %in% colnames(policingdata))) {
          observed <- policingdata %>%
            filter(qol == TRUE) %>%
            group_by(Race, Gender) %>%
            summarise(people = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(people = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          expected <- acsracegender %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
          
          qdata <- observed %>%
            mutate(proportion = prop.table(people)) %>%
            select(-people) %>%
            mutate(datatype = "Policing Records") %>%
            rbind(acsracegender) %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records")) %>%
            filter(Gender != "Missing gender data") %>% 
            filter(Race != "Missing race data") 
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) + 
            geom_bar(stat = "identity", position = "stack") + 
            xlab("Reported Race") + 
            ylab("Proportion") +
            scale_fill_manual(values = c("coral", "blue"),  
                              guide = guide_legend(title = "Reported Gender"), 
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.3, 0.7), 
                               guide = guide_legend(title = "Data Type"),
                               labels = c("Policing Records", "Local Population")) + 
            facet_wrap(~Gender)+  
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          globalVars$p4 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
          
          globalVars$t4 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          deleteData(wb, sheet = question, cols = 1, rows = 2)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
          writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
          #writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data") 
          mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
          mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
          #mergeCells(wb, sheet = question, rows = 1, cols = 6:7) 
          writeData(wb, sheet = question, startRow = 2, startCol = 2, "Policing")
          writeData(wb, sheet = question, startRow = 2, startCol = 4, "Policing")
          #writeData(wb, sheet = question, startRow = 2, startCol = 6, "Policing") 
          writeData(wb, sheet = question, startRow = 2, startCol = 3, "Population")
          writeData(wb, sheet = question, startRow = 2, startCol = 5, "Population")
          #writeData(wb, sheet = question, startRow = 2, startCol = 7, "Population") 
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
          if (chisq$p.value < 0.05) {
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
            globalVars$m4 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          } else{
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions do not appear to differ significantly from the population proportions."
            globalVars$m4 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q4_interp <- renderUI({
            text <- "Data for question Q4 is missing."
            return(tags$p(text))
          })
          globalVars$t4 <- NULL
        }
        
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 5 of 13...", footer=NULL))
        question <- 5
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "Arrest") %in% colnames(policingdata))) {
          
          # Racial breakdown of arrests
          observed <- policingdata %>%
            filter(Arrest == TRUE) %>%
            group_by(Race, Gender) %>%
            summarise(people = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(people = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          expected <- acsracegender %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            filter(Race != "Missing race data") %>%
            filter(Gender != "Missing gender data") %>%
            arrange(Race, Gender)
          
          chisq <- chisq.test(x = observed$people, p = expected$proportion, simulate.p.value = TRUE)
          
          qdata <- observed %>%
            mutate(proportion = prop.table(people)) %>%
            select(-people) %>%
            mutate(datatype = "Policing Records") %>%
            rbind(acsracegender) %>%
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records")) %>%
            filter(Gender != "Missing gender data") %>%
            filter(Race != "Missing race data")
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) +
            geom_bar(stat = "identity", position = "stack") +
            xlab("Reported Race") + 
            ylab("Proportion") + 
            scale_fill_manual(values = c("coral", "blue"),
                              guide = guide_legend(title = "Reported Gender"), 
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.3, 0.7),
                               guide = guide_legend(title = "Data Type"),
                               labels = c("Policing Records", "Local Population")) +
            facet_wrap(~Gender)+ 
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          globalVars$p5 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender","datatype"), values_from = "proportion") 
          
          globalVars$t5 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          deleteData(wb, sheet = question, cols = 1, rows = 2)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Man")
          writeData(wb, sheet = question, startRow = 1, startCol = 4, "Woman")
          #writeData(wb, sheet = question, startRow = 1, startCol = 6, "Missing gender data") 
          mergeCells(wb, sheet = question, rows = 1, cols = 2:3)
          mergeCells(wb, sheet = question, rows = 1, cols = 4:5)
          #mergeCells(wb, sheet = question, rows = 1, cols = 6:7) 
          writeData(wb, sheet = question, startRow = 2, startCol = 2, "Population")
          writeData(wb, sheet = question, startRow = 2, startCol = 4, "Population")
          #writeData(wb, sheet = question, startRow = 2, startCol = 6, "Population") 
          writeData(wb, sheet = question, startRow = 2, startCol = 3, "Policing")
          writeData(wb, sheet = question, startRow = 2, startCol = 5, "Policing")
          #writeData(wb, sheet = question, startRow = 2, startCol = 7, "Policing") 
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:7, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:11, cols = 2:7, gridExpand = TRUE)
          if (chisq$p.value < 0.05) {
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions appear to differ significantly from the population proportions."
            globalVars$m5 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }else{
            mergeCells(wb, sheet = question, rows = 13, cols = 1:7)
            qmessage <- "The policing data proportions do not appear to differ significantly from the population proportions."
            globalVars$m5 <- qmessage
            writeData(wb, sheet = question, startRow = 13, startCol = 1, qmessage)
          }
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q5_interp <- renderUI({
            text <- "Data for question Q5 is missing."
            return(tags$p(text))
          })
          globalVars$t5 <- NULL
        }
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 6 of 13...", footer=NULL))
        question <- 6
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "Arrest") %in% colnames(policingdata))) {
          # Proportion arrests for different gender/race
          
          # Note: we treat proportions as 0 when the calculation is 0/0
          qdata_policing <- policingdata %>%
            group_by(Race, Gender) %>%
            summarise(arrests = sum(Arrest), incidents = n()) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(arrests = 0, incidents = 0)) %>%
            mutate(proportion = arrests/(incidents + .Machine$double.eps)) %>%
            select(-arrests, -incidents) %>%
            filter(Gender != "Missing gender data") %>%  
            filter(Race != "Missing race data") %>%
            mutate(datatype = "Policing Records") 
          
          qdata_population <- acsracegender %>% 
            group_by(Race, Gender) %>% 
            mutate(datatype = "Local Population") %>%
            arrange(Race, Gender)
          
          qdata_policing$proportion <- qdata_policing$proportion * qdata_population$proportion
          
          qdata <- bind_rows(qdata_policing, qdata_population)
          
          qdata <- qdata %>% 
            complete(Race, Gender, datatype, fill = list(proportion = 0)) %>%
            mutate(datatype = factor(datatype)) %>%
            mutate(datatype = relevel(datatype, ref = "Policing Records"))  %>% 
            filter(Race != "Missing race data") %>% 
            filter(Gender != "Missing gender data") 
          
          p <- qdata %>%
            ggplot(aes(x = Race, y = proportion, fill = Gender, alpha = datatype)) +
            geom_bar(stat = "identity", position = "stack") + 
            scale_fill_manual(values = c("coral", "blue"),
                              guide = guide_legend(title = "Reported Gender", order = 1), 
                              labels = c("Man", "Women")) + 
            scale_alpha_manual(values = c(0.3, 0.7),
                               guide = guide_legend(title = "Data Type", order = 2), 
                               labels = c("Policing Records", "Local Population")) +
            xlab("Reported Race") + 
            ylab("Proportion") + 
            facet_wrap(~Gender)+ 
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15)) 
          
          globalVars$p6 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender", "datatype"), values_from = "proportion") 
          
          globalVars$t6 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:4, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:4, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:4, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q6_interp <- renderUI({
            text <- "Data for question Q6 is missing."
            return(tags$p(text))
          })
          globalVars$t6 <- NULL
        }
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 7 of 13...", footer=NULL))
        question <- 7
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "BondAmount") %in% colnames(policingdata))) {
          
          # Bond amount
          qdata <- policingdata %>%
            group_by(Race, Gender) %>%
            summarise(meanbond = mean(BondAmount, na.rm = TRUE)) %>%
            ungroup %>%
            complete(Race, Gender, fill = list(meanbond = NA)) %>%
            filter(Gender != "Missing gender data") %>% 
            filter(Race != "Missing race data")
          
          
          p <- qdata %>%
            ggplot(aes(x = meanbond, y = Race)) +
            geom_col(aes(fill=Gender), position = "dodge", width = .8) +
            ylab("Reported Race") +
            xlab("Mean Bond Amount") +
            scale_fill_manual(values = c("skyblue", "grey30"), name = "Reported Gender") +
            theme_bw() +
            theme(legend.position = "bottom", text = element_text(size = 15))  
          
          
          globalVars$p7 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender"), values_from = "meanbond") 
          
          globalVars$t7 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:4, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:4, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded2, rows = 3:(nrow(qExcel) + 2), cols = 2:4, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q7_interp <- renderUI({
            text <- "Data for question Q7 is missing."
            return(tags$p(text))
          })
          globalVars$t7 <- NULL
        }
        
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 8 of 13...", footer=NULL))
        question <- 8
        
        # Check if data is available for analysis
        if (all(c("Patrol", "Race") %in% colnames(policingdata))) {
          
          modifiedpolicingdata <- policingdata
          
          modifiedpolicingdata$Race <- ifelse(modifiedpolicingdata$Race %in% c("White", "Black"), modifiedpolicingdata$Race, "Other")
          
          qdata <- modifiedpolicingdata %>%
            group_by(Patrol, Race) %>%
            summarise(count = n()) %>%
            ungroup %>%
            complete(Patrol, Race, fill = list(count = 0)) %>%
            group_by(Patrol) %>%
            mutate(proportion = prop.table(count)) %>%
            filter(Race != "Missing race data") ###### ADDED
          
          
          # Reorder Race within each Patrol group based on proportion
          qdata <- qdata %>%
            arrange(Patrol, desc(proportion)) %>%
            mutate(Race = factor(Race, levels = Race))
          
          modifiedqdata <- qdata
          
          # Manually map these codes to "White", "Black", or "Other"
          modifiedqdata$Race <- dplyr::recode(modifiedqdata$Race,
                                              `3` = "White",  
                                              `6` = "Black", 
                                              .default = "Other") 
          
          p <- ggplot() +
            geom_col(data = modifiedqdata, aes(x = Patrol, y = proportion), fill="skyblue") +
            ylab("Proportion")+
            facet_wrap(~Race)+
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1), text = element_text(size = 15))
          
          globalVars$p8 <- p
          
          qExcel <- modifiedqdata %>%
            select(-count) %>%
            pivot_wider(names_from = c("Race"), values_from = "proportion") 
          
          globalVars$t8 <- qExcel
          
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Race")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:10)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q8_interp <- renderUI({
            text <- "Data for question Q8 is missing."
            return(tags$p(text))
          })
          globalVars$t8 <- NULL
        }
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 9 of 13...", footer=NULL))
        question <- 9
        
        # Check if data is available for analysis
        if (all(c("Officer", "Race") %in% colnames(policingdata))) {
          
          # Racial breakdown for different officers
          # We will look at top 25% of officers or top 10 officers, whichever is a shorter list
          
          officertally <- policingdata %>%
            group_by(Officer) %>%
            summarise(count = n()) %>%
            arrange(desc(count))
          
          top25percent <- officertally %>%
            filter(count >= quantile(count, 0.75)) %>%
            pull(Officer)
          
          top10 <- officertally %>%
            .[1:10, ] %>%
            pull(Officer)  
          
          officerlist <- intersect(top25percent, top10)
          
          
          qdata <- modifiedpolicingdata %>%
            filter(Officer %in% officerlist) %>%
            mutate(Officer = droplevels(Officer)) %>%
            group_by(Officer, Race) %>%
            summarise(count = n()) %>%
            ungroup %>%
            complete(Officer, Race, fill = list(count = 0)) %>%
            group_by(Officer) %>%
            mutate(proportion = prop.table(count)) %>%
            filter(Race != "Missing race data") 
          
          modified9qdata <- qdata          
          #Manually map these codes to "White", "Black", or "Other"
          modified9qdata$Race <- dplyr::recode(modified9qdata$Race,
                                               `3` = "Black",  
                                               `6` = "White", 
                                               .default = "Other") 
          

          p <- ggplot() +
            geom_col(data = modified9qdata, aes(x = Officer, y = proportion), fill="skyblue") +
            ylab("Proportion") +
            facet_wrap(~Race)+
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1), text = element_text(size = 15))
          
          globalVars$p9 <- p
          
          
          qExcel <- modified9qdata %>%
            select(-count) %>%
            pivot_wider(names_from = c("Race"), values_from = "proportion") 
          
          globalVars$t9 <- qExcel
          
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Race")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:10)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q9_interp <- renderUI({
            text <- "Data for question Q9 is missing."
            return(tags$p(text))
          })
          globalVars$t09 <- NULL
        }
        
        #########
        ### Q ###
        #########
        removeModal()
        showModal(modalDialog("Analyzing Question 10 of 13...", footer=NULL))
        
        question <- 10
        
        # Check if data is available for analysis
        if (all(c("Day", "Time") %in% colnames(policingdata))) {
          qdata <- policingdata %>%
            select(Day, Time) %>%
            mutate(Time = hour(Time)) %>%
            group_by(Day, Time) %>%
            summarise(count = n()) %>%
            mutate(Day = factor(Day, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>%
            mutate(Time = as.factor(Time)) 
          
          day_colors <- c( 
            "Mon" = "darkblue", 
            "Tue" = "#ff7f0e",  
            "Wed" = "darkgreen",  
            "Thu" = "grey30",  
            "Fri" = "yellow",  
            "Sat" = "magenta",  
            "Sun" = "#17becf"   
          )
          
          every_nth = function(n) {
            return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
          }
          
          p <- qdata %>%
            ggplot(aes(x = Time, y = count, group = Day, color = Day)) +
            geom_line(size = 1.5, alpha = 0.7) + 
            scale_color_manual(values = day_colors) + 
            scale_y_continuous(name = "Number of Incidents") +
            scale_x_discrete(name = "Hour of Day",breaks = every_nth(n = 3)) +
            facet_wrap(~Day)+
            theme(panel.grid.minor.y = element_blank())
          
          globalVars$p10 <- p
          
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Time"), values_from = "count") %>%
            ungroup()%>%
            mutate(newSum = select_if(., is.numeric) %>% reduce(`+`)) %>% 
            mutate_if(is.numeric, list(~ ./newSum)) %>% 
            select(-newSum)
          
          globalVars$t10 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Hour")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:25)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:25, gridExpand = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:9, cols = 2:25, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q10_interp <- renderUI({
            text <- "Data for question Q10 is missing."
            return(tags$p(text))
          })
          globalVars$t10 <- NULL
        }
        
        #########
        ### Q ###
        #########
        
        removeModal() 
        showModal(modalDialog("Analyzing Question 11 of 13...", footer=NULL))
        
        question <- 11
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "UseOfForce") %in% colnames(policingdata))) {
          qdata <- policingdata %>%
            filter(UseOfForce == "Yes") %>%
            group_by(Race, Gender) %>%
            summarise(incidents = n(), .groups = 'drop') %>%
            ungroup() %>%
            complete(Race, Gender, fill = list(incidents = 0)) %>%
            filter(Race != "Missing race data" & Gender != "Missing gender data")
          
          p <- qdata %>%
            ggplot(aes(x = incidents, y = Race)) +
            geom_col(aes(fill=Gender), position = "dodge", width=0.8) +
            ylab("Reported Race") +
            xlab("Number of Incidents") +
            scale_fill_manual(values = c("skyblue", "grey30"), name = "Reported Gender") +
            theme_bw() +
            theme(legend.position = "bottom", text = element_text(size = 15))
          
          globalVars$p11 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender"), values_from = "incidents") 
          
          globalVars$t11 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:10)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q11_interp <- renderUI({
            text <- "Data for question Q11  is missing."
            return(tags$p(text))
          })
          globalVars$t11 <- NULL
        }
        
        #########
        ### Q ###
        #########
        removeModal() 
        showModal(modalDialog("Analyzing Question 12 of 13...", footer=NULL))
        
        question <- 12
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "Stopped") %in% trimws(colnames(policingdata)))) {
          qdata <- policingdata %>%
            filter(Stopped == "Yes") %>%
            group_by(Race, Gender) %>%
            summarise(incidents = n(), .groups = 'drop') %>%
            ungroup() %>%
            complete(Race, Gender, fill = list(incidents = 0)) %>%
            filter(Race != "Missing race data" & Gender != "Missing gender data")
          
          
          p <- qdata %>%
            ggplot(aes(x = incidents, y = Race)) +
            geom_col(aes(fill=Gender), position = "dodge", width=0.8) +
            ylab("Reported Race") +
            xlab("Number of Incidents") +
            scale_fill_manual(values = c("skyblue", "grey30"), name = "Reported Gender") +
            theme_bw() +
            theme(legend.position = "bottom", text = element_text(size = 15))
          
          globalVars$p12 <- p
          
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender"), values_from = "incidents")
          
          globalVars$t12 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q12_interp <- renderUI({
            text <- "Data for question Q12 is missing."
            return(tags$p(text))
          })
          globalVars$t12 <- NULL
        }
        
        #########
        ### Q ###
        #########
        removeModal() 
        showModal(modalDialog("Analyzing Question 13 of 13...", footer=NULL)) ###### ADDED!
        
        question <- 13
        
        # Check if data is available for analysis
        if (all(c("Race", "Gender", "SearchConducted") %in% colnames(policingdata))) {
          # Data available, proceed with analysis (existing code for question 13)
          qdata <- policingdata %>%
            filter(SearchConducted == "Yes") %>%
            group_by(Race, Gender) %>%
            summarise(incidents = n(), .groups = 'drop') %>%
            ungroup() %>%
            complete(Race, Gender, fill = list(incidents = 0)) %>%
            filter(Race != "Missing race data" & Gender != "Missing gender data")
          
          p <- qdata %>%
            ggplot(aes(x = incidents, y = Race)) +
            geom_col(aes(fill=Gender), position = "dodge", width=0.8) +
            ylab("Reported Race") +
            xlab("Number of Searches") +
            scale_fill_manual(values = c("skyblue", "grey30"), name = "Reported Gender") +
            theme_bw() +
            theme(legend.position = "bottom", text = element_text(size = 15))
          
          globalVars$p13 <- p
          
          qExcel <- qdata %>%
            pivot_wider(names_from = c("Gender"), values_from = "incidents")
          
          globalVars$t13 <- qExcel
          
          writeData(wb, sheet = question, x = qExcel, startRow = 2, borderStyle = openxlsx_getOp("borderStyle", "none"), headerStyle = NULL)
          writeData(wb, sheet = question, startRow = 1, startCol = 2, "Gender")
          mergeCells(wb, sheet = question, rows = 1, cols = 2:4)
          addStyle(wb, sheet = question, style = centered, rows = 1:2, cols = 2:10, gridExpand = TRUE)
          setColWidths(wb, sheet = question, cols = 1, widths = "auto", ignoreMergedCells = TRUE)
          setColWidths(wb, sheet = question, cols = 2:10, widths = 20, ignoreMergedCells = TRUE)
          addStyle(wb, sheet = question, style = centeredrounded3, rows = 3:(nrow(qExcel) + 2), cols = 2:10, gridExpand = TRUE)
        } else {
          # Data missing, inform user and skip analysis
          output$Q13_interp <- renderUI({
            text <- "Data for question Q13 (Race, Gender and SearchConducted variable in policing data) is missing."
            return(tags$p(text))
          })
          globalVars$t13 <- NULL
        }
        
        ########################
        ### SAVE EXCEL SHEET ###
        ########################
        removeModal()
        globalVars$wb <- wb
        globalVars$changed <- FALSE
        updateUI(isolate(globalVars$changed))
      },
      error=function(e){
        removeModal()
        shinyalert("Oops!", "Something went wrong. Check your census api key, and ensure your date and time column is formatted as Month-Day-Year-Hour-Minute compatible.", type = "error")
      }
    )
  }
  
  
  ##############################################################################################################
  # Download Results
  ##############################################################################################################    
  output$downloadresultsZip <- downloadHandler(
    filename="SToPA Tookit.zip",
    
    if(globalVars$clean){
      content = function(file){
        ggsave('q01.png', plot=globalVars$p1,  width = 6.5, units = "in")
        ggsave('q02.png', plot=globalVars$p2,  width = 6.5, units = "in")
        ggsave('q03.png', plot=globalVars$p3,  width = 6.5, units = "in")
        ggsave('q04.png', plot=globalVars$p4,  width = 6.5, units = "in")
        ggsave('q05.png', plot=globalVars$p5,  width = 6.5, units = "in")
        ggsave('q06.png', plot=globalVars$p6,  width = 6.5, units = "in")
        ggsave('q07.png', plot=globalVars$p7,  width = 6.5, units = "in")
        ggsave('q08.png', plot=globalVars$p8,  width = 6.5, units = "in")
        ggsave('q09.png', plot=globalVars$p9,  width = 6.5, units = "in")
        ggsave('q10.png', plot=globalVars$p10, width = 6.5, units = "in")
        ggsave('q11.png', plot=globalVars$p11, width = 6.5, units = "in") ###### ADDED?
        ggsave('q12.png', plot=globalVars$p12, width = 6.5, units = "in") ###### ADDED?
        ggsave('q13.png', plot=globalVars$p13, width = 6.5, units = "in") ###### ADDED?
        saveWorkbook(globalVars$wb, "SToPA Tookit.xlsx", overwrite = TRUE)
        
        zip::zip(file, files = c(paste("q0", 1:12, ".png", sep=""), "q13.png", "SToPA Tookit.xlsx") ) ###### ADDED?
      }
    }else{
      content = function(file){
        ggsave('q01.png', plot=globalVars$p1,  width = 6.5, units = "in")
        ggsave('q02.png', plot=globalVars$p2,  width = 6.5, units = "in")
        ggsave('q03.png', plot=globalVars$p3,  width = 6.5, units = "in")
        ggsave('q04.png', plot=globalVars$p4,  width = 6.5, units = "in")
        ggsave('q05.png', plot=globalVars$p5,  width = 6.5, units = "in")
        ggsave('q06.png', plot=globalVars$p6,  width = 6.5, units = "in")
        ggsave('q07.png', plot=globalVars$p7,  width = 6.5, units = "in")
        ggsave('q08.png', plot=globalVars$p8,  width = 6.5, units = "in")
        ggsave('q09.png', plot=globalVars$p9,  width = 6.5, units = "in")
        ggsave('q10.png', plot=globalVars$p10, width = 6.5, units = "in")
        ggsave('q11.png', plot=globalVars$p11, width = 6.5, units = "in") ###### ADDED?
        ggsave('q12.png', plot=globalVars$p12, width = 6.5, units = "in") ###### ADDED?
        ggsave('q13.png', plot=globalVars$p13, width = 6.5, units = "in") ###### ADDED?
        saveWorkbook(globalVars$wb, "SToPA Tookit.xlsx", overwrite = TRUE)
        write.csv(x = globalVars$dataset, file = "CleanedInputFile.csv", row.names = F)
        zip::zip(file, files = c(paste("q0", 1:12, ".png", sep=""), "q13.png", "SToPA Tookit.xlsx") ) ###### ADDED?
      } 
    }
  )
  
  ##############################################################################
  ###       Q1         #########################################################
  ##############################################################################
  output$Q1_plot <- renderPlot({
    req(globalVars$p1)
    print(globalVars$p1)
  })
  
  output$Q1_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q01.", input$Q1_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p1, device = input$Q1_format, width = as.numeric(input$Q1_width),
             height = as.numeric(input$Q1_height), units = input$Q1_unit
      )
    }
  )
  
  output$Q1_tab <- DT::renderDataTable(
    {
      globalVars$t1 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q01"
        ))
    )
  )
  
  output$Q1_interp <- renderUI({
    text <- paste("\U2022", globalVars$m1)
    withMathJax(tags$p(HTML(text)))
    
  })
  
  ##############################################################################
  ###       Q2         #########################################################
  ##############################################################################
  output$Q2_plot <- renderPlot({
    req(globalVars$p2)
    print(globalVars$p2)
  })
  
  output$Q2_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q02.", input$Q2_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p2, device = input$Q2_format, width = as.numeric(input$Q2_width),
             height = as.numeric(input$Q2_height), units = input$Q2_unit
      )
    }
  )
  
  output$Q2_tab <- DT::renderDataTable(
    {
      globalVars$t2 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q02"
        ))
    )
  )
  
  output$Q2_interp <- renderUI({
    text <- paste("\U2022", globalVars$m2)
    withMathJax(tags$p(HTML(text)))
    
  })
  
  ##############################################################################
  ###       Q3         #########################################################
  ##############################################################################
  output$Q3_plot <- renderPlot({
    req(globalVars$p3)
    print(globalVars$p3)
  })
  
  output$Q3_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q03.", input$Q3_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p3, device = input$Q3_format, width = as.numeric(input$Q3_width),
             height = as.numeric(input$Q3_height), units = input$Q3_unit
      )
    }
  )
  
  output$Q3_tab <- DT::renderDataTable(
    {
      globalVars$t3 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q03"
        ))
    )
  )
  
  output$Q3_interp <- renderUI({
    text <- paste("\U2022", globalVars$m3)
    withMathJax(tags$p(HTML(text)))
    
  })
  
  ##############################################################################
  ###       Q4         #########################################################
  ##############################################################################
  output$Q4_plot <- renderPlot({
    req(globalVars$p4)
    print(globalVars$p4)
  })
  
  output$Q4_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q04.", input$Q4_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p4, device = input$Q4_format, width = as.numeric(input$Q4_width),
             height = as.numeric(input$Q4_height), units = input$Q4_unit
      )
    }
  )
  
  output$Q4_tab <- DT::renderDataTable(
    {
      globalVars$t4 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q04"
        ))
    )
  )
  
  output$Q4_interp <- renderUI({
    text <- paste("\U2022", globalVars$m4)
    withMathJax(tags$p(HTML(text)))
    
  })
  
  ##############################################################################
  ###       Q5         #########################################################
  ##############################################################################
  output$Q5_plot <- renderPlot({
    req(globalVars$p5)
    print(globalVars$p5)
  })
  
  output$Q5_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q05.", input$Q5_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p5, device = input$Q5_format, width = as.numeric(input$Q5_width),
             height = as.numeric(input$Q5_height), units = input$Q5_unit
      )
    }
  )
  
  output$Q5_tab <- DT::renderDataTable(
    {
      globalVars$t5 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q05"
        ))
    )
  )
  
  output$Q5_interp <- renderUI({
    text <- paste("\U2022", globalVars$m5)
    withMathJax(tags$p(HTML(text)))
    
  })
  
  ##############################################################################
  ###       Q6         #########################################################
  ##############################################################################
  output$Q6_plot <- renderPlot({
    req(globalVars$p6)
    print(globalVars$p6)
  })
  
  output$Q6_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q06.", input$Q6_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p6, device = input$Q6_format, width = as.numeric(input$Q6_width),
             height = as.numeric(input$Q6_height), units = input$Q6_unit
      )
    }
  )
  
  output$Q6_tab <- DT::renderDataTable(
    {
      globalVars$t6 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q06"
        ))
    )
  )
  
  ##############################################################################
  ###       Q7         #########################################################
  ##############################################################################
  output$Q7_plot <- renderPlot({
    req(globalVars$p7)
    print(globalVars$p7)
  })
  
  output$Q7_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q07.", input$Q7_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p7, device = input$Q7_format, width = as.numeric(input$Q7_width),
             height = as.numeric(input$Q7_height), units = input$Q7_unit
      )
    }
  )
  
  output$Q7_tab <- DT::renderDataTable(
    {
      globalVars$t7 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q07"
        ))
    )
  )
  
  ##############################################################################
  ###       Q8         #########################################################
  ##############################################################################
  output$Q8_plot <- renderPlot({
    req(globalVars$p8)
    print(globalVars$p8)
  })
  
  output$Q8_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q08.", input$Q8_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p8, device = input$Q8_format, width = as.numeric(input$Q8_width),
             height = as.numeric(input$Q8_height), units = input$Q8_unit
      )
    }
  )
  
  output$Q8_tab <- DT::renderDataTable(
    {
      globalVars$t8 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q08"
        ))
    )
  )
  
  ##############################################################################
  ###       Q9         #########################################################
  ##############################################################################
  output$Q9_plot <- renderPlot({
    req(globalVars$p9)
    print(globalVars$p9)
  })
  
  output$Q9_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q09.", input$Q9_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p9, device = input$Q9_format, width = as.numeric(input$Q9_width),
             height = as.numeric(input$Q9_height), units = input$Q9_unit
      )
    }
  )
  
  output$Q9_tab <- DT::renderDataTable(
    {
      globalVars$t9 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q09"
        ))
    )
  )
  
  ##############################################################################
  ###       Q10         #########################################################
  ##############################################################################
  output$Q10_plot <- renderPlot({
    req(globalVars$p10)
    print(globalVars$p10)
  })
  
  output$Q10_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q10.", input$Q10_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p10, device = input$Q10_format, width = as.numeric(input$Q10_width),
             height = as.numeric(input$Q10_height), units = input$Q10_unit
      )
    }
  )
  
  output$Q10_tab <- DT::renderDataTable(
    {
      globalVars$t10 %>% mutate(across(where(is.numeric), round, 6))
    },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q10"
        ))
    )
  )
  
  ############################################################################## ADDED?
  ###       Q11         #########################################################
  ##############################################################################
  output$Q11_plot <- renderPlot({
    req(globalVars$p11)
    print(globalVars$p11)
  })
  
  output$Q11_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q11.", input$Q11_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p11, device = input$Q11_format, width = as.numeric(input$Q11_width),
             height = as.numeric(input$Q11_height), units = input$Q11_unit
      )
    }
  )
  
  # Update renderDataTable to handle missing data
  output$Q11_tab <- DT::renderDataTable({
    # Check if data is available before processing
    if (!is.null(globalVars$t11)) {
      globalVars$t11 %>% mutate(across(where(is.numeric), round, 6))
    } else {
      # Show a message if data is missing
      return(data.frame(Message = "Data for question 11 is missing. No table available."))
    }
  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(
    dom = "Bfrtip",
    buttons =
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend = "csv", filename = "prop-test-summary"),
          list(extend = "excel", filename = "prop-test-summary"),
          list(extend = "pdf", filename = "prop-test-summary")
        ),
        text = "Download",
        filename = "q11"
      ))
   )
  )
  
  ############################################################################## ADDED?
  ###       Q12         #########################################################
  ##############################################################################
  output$Q12_plot <- renderPlot({
    req(globalVars$p12)
    print(globalVars$p12)
  })
  
  output$Q12_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q12.", input$Q12_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p12, device = input$Q12_format, width = as.numeric(input$Q12_width),
             height = as.numeric(input$Q12_height), units = input$Q12_unit
      )
    }
  )
  
  # Update renderDataTable to handle missing data
  output$Q12_tab <- DT::renderDataTable({
    # Check if data is available before processing
    if (!is.null(globalVars$t12)) {
      globalVars$t12 %>% mutate(across(where(is.numeric), round, 6))
    } else {
      # Show a message if data is missing
      return(data.frame(Message = "Data for question 12 is missing. No table available."))
    }
  },
    extensions = "Buttons",
    rownames = FALSE,
    options = list(
      dom = "Bfrtip",
      buttons =
        list("copy", "print", list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "prop-test-summary"),
            list(extend = "excel", filename = "prop-test-summary"),
            list(extend = "pdf", filename = "prop-test-summary")
          ),
          text = "Download",
          filename = "q12"
        ))
    )
  )
  
  
  
  
  ############################################################################## ADDED?
  ###       Q13         #########################################################
  ##############################################################################
  output$Q13_plot <- renderPlot({
    req(globalVars$p13)
    print(globalVars$p13)
  })
  
  output$Q13_downloadPlot <- downloadHandler(
    filename = function() {
      paste("q13.", input$Q13_format, sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = globalVars$p13, device = input$Q13_format, width = as.numeric(input$Q13_width),
             height = as.numeric(input$Q13_height), units = input$Q13_unit
      )
    }
  )
  
  # Update renderDataTable to handle missing data
  output$Q13_tab <- DT::renderDataTable({
    # Check if data is available before processing
    if (!is.null(globalVars$t13)) {
      globalVars$t13 %>% mutate(across(where(is.numeric), round, 6))
    } else {
      # Show a message if data is missing
      return(data.frame(Message = "Data for question 13 is missing. No table available."))
    }
  },
  extensions = "Buttons",
  rownames = FALSE,
  options = list(
    dom = "Bfrtip",
    buttons =
      list("copy", "print", list(
        extend = "collection",
        buttons = list(
          list(extend = "csv", filename = "prop-test-summary"),
          list(extend = "excel", filename = "prop-test-summary"),
          list(extend = "pdf", filename = "prop-test-summary")
        ),
        text = "Download",
        filename = "q13"
      ))
   )
  )
})