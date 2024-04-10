# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################
# QSIDE app 
# @author The Data Science Collaboratory At Colgate University (datascience[at]gmail[dot]com)
# @description UI code for t-test app
# #################################################################################################
# #################################################################################################
# #################################################################################################
# #################################################################################################

##########################################
# Shiny
##########################################
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinymeta)
library(shinythemes)
library(shinyAce)
library(shinyBS)
library(DT)
library(zip)

##########################################
# General
##########################################
library(tidyverse) #ggplot2, tibble, tidyr, reader, #purrr, dplyr, stringr, forcats

##########################################
# QSIDE
##########################################
library(tigris)
library(sf)
library(tidycensus)
library(labelled)
library(openxlsx)
library(lubridate)
library(hms)


#--------------------------------------------
#---------------     UI    ------------------
#--------------------------------------------
ui<-tagList(tags$head(tags$link(rel = "icon", type = "image/x-icon",
                                href = "https://pbs.twimg.com/profile_images/1118667730067103744/E2koq4n1_400x400.png"),
                      tags$style(HTML(".paragraph {margin:auto;max-width: 50%;font-size: 15px; text-align:justify;}
                                        h1 {text-align:center;}"))),
            useShinyjs(),
            useShinyalert(),
            
            navbarPage(title="QSIDE SToPA (Small Town Policing Accountability) Application", id = "tabs",
                       theme = shinytheme("flatly"),
                       
                       # About page
                       tabPanel("About",
                                h1("QSIDE SToPA App", align="center"),
                                tags$div(class = "paragraph", tags$hr(),
                                         p("Welcome to the QSIDE SToPA Data Application. In order to process policing data, please click on the Analyze Data Tab and upload a CSV file. Currently, the software does not accept any other file formats. Once you have uploaded your data, select the State, County, and Municipality of the town from which the data was pulled. The program may take a few minutes to process once you have submitted your data. Please note that in order to use this dataset, your dataframe must contain the following variables: Gender, Race, Traffic Data, Drug/Firearm Possession, Quality of Life Violations, Arrest (Yes/No), Bond Amount, Patrol, Officer Name, and Date/Time of incident.")), 
                                br(),
                                HTML(paste0(
                                  "<h2 align='center'>Tutorial Instructions</h2>",
                                  "<div class='paragraph'><hr>",
                                  "<ul>",
                                  "<li>Click on the “Analyze Data” tab on the top banner.</li>",
                                  "<li>On the upper lefthand side of the screen, click the “Sample Dataset” button, which will automatically populate a majority of the page. PLEASE NOTE THIS DATASET IS COMPOSED OF BOTH REAL AND SYNTHETIC DATA. THIS IS NOT REPRRSENTATIVE OF REAL POLICING DATA FROM DURHAM, NC.</li>",
                                  "<li>Scroll down to the heading that reads, “Geographic Input.” Select the State, County, and Town from where your data is sourced. In the case of the sample dataset, this will be the following: NC, Durham, and Durham.</li>",
                                  "<li>Click the button that says “Complete Analysis” at the bottom of the page.</li>",
                                  "<li>Wait for the data to be run through the internal processing in the application. From there, a data preview as well as a question-by-question breakdown are available.</li>",
                                  "</ul>",
                                  "</div>"
                                )),
                                HTML(
                                  paste0(
                                    "<h2 align='center'>Data Dictionary</h2>",
                                    "<div class='paragraph'><hr>",
                                    "<ul>",
                                    "<li>Race: (Categorical/Nominal) - Race recorded in police report</li>",
                                    "<li>Gender: (Categorical/Nominal) - Gender recorded in police report</li>",
                                    "<li>Arrest: (Boolean) - Indicator of whether the individual was arrested</li>",
                                    "<li>Bond Amount: (Ratio/Continuous) - If arrested, recorded bond amount</li>",
                                    "<li>Patrol: (Categorical/Nominal) - Officer’s corresponding patrol unit</li>",
                                    "<li>Officer: (Categorical/Nominal) - Name and number of the primary officer</li>",
                                    "<li>DateTime: (TimeStamp/DateTime) - Date and time incident information</li>",
                                    "<li>Selected Charge 1: (Categorical/Nominal) - Charge recorded in incident report</li>",
                                    "<li>Timezone: (Categorical/Nominal) - Timezone of the city in which the incident took place</li>",
                                    "<li>Use of Force: (Categorical/Nominal) - Indicator of use of force</li>",
                                    "<li>Search Conducted: (Categorical/Nominal) - Indicator of whether a search was conducted</li>",
                                    "<li>Stopped: (Categorical/Nominal) - Indicator of whether there was a stop</li>",
                                    "</ul>",
                                    "</div>"
                                  )
                                ),
                                br(),
                                HTML(
                                  paste0(
                                    "<h2 align='center'>Core Questions</h2>",
                                    "<div class='paragraph'><hr>",
                                    "<ul>",
                                    "<li> Q1- What proportion of all police incidents go to different gender/race groups? </li>",
                                    "<li>Q2- What proportion of incidents related to routine traffic/moving violations go to different gender/race groups?</li>",
                                    "<li>Q3- What proportion of incidents related to drug and/or firearm possession go to different gender/race groups?</li>",
                                    "<li>Q4- What proportion of incidents related to quality-of-life violations go to different gender/race groups?</li>",
                                    "<li>Q5- What proportion of incidents that resulted in an arrest go to different gender/race groups? </li>",
                                    "<li>Q6- What proportion of incidents that occured resulted in an arrest for different gender/race groups?</li>",
                                    "<li>Q7- How does average bond amount differ according to race and gender?</li>",
                                    "<li>Q8- What proportion of police incidents go to different race groups within each patrolling beat/neighborhood?</li>",
                                    "<li>Q9- What proportion of police incidents go to different race groups for each of the most active officers?</li>",
                                    "<li>Q10- What number of police incidents occur during each weekday/hour of day?</li>",
                                    "<li>Q11- Are officers more likely to use force on subjects from specific gender/race groups?</li>",
                                    "<li>Q12- Are subjects from specific gender/race groups stopped in street checks at higher rates?</li>",
                                    "<li>Q13- Are subjects from specific gender/race groups more likely to be searched if they are stopped in a street check?</li>",
                                    "</ul>",
                                    "</div>"
                                  )
                                ),
                                br(), br(), br()
                       ),
                       #Dataset exploration panel
                       tabPanel("Analyze Data",
                                #visualize dataset and display dataset
                                mainPanel(
                                  tabsetPanel(id="main",
                                              
                                              tabPanel("Data Input", br(), value = "datainput",
                                                       fluidPage(
                                                         fileInput("file_upload", "Upload a File",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                         actionButton("sample", "Sample dataset"),
                                                         hidden(div(id='choose_sample', 
                                                                    selectInput("sample_data_choice","Sample Data:", 
                                                                                choices = c("Durham NC"),
                                                                                selected = "Durham NC"))),
                                                         tags$hr(),
                                                         h3("Racial Variables"),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectizeInput("select_race_column",
                                                                                 "Race Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectizeInput("select_aian",
                                                                                 "Am. Indian/Alaska Native Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                                  ),
                                                           column(width=3, 
                                                                  selectizeInput("select_asian",
                                                                                 "Asian Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3, 
                                                                  selectizeInput("select_black",
                                                                                 "Black / African American Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3,
                                                                  selectizeInput("select_hispanic",
                                                                                 "Hispanic / LatinX Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           br(),
                                                           column(width=3,
                                                                  selectizeInput("select_nhpi",
                                                                                 "Native Hawaiin / Pacific Islander",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3, 
                                                                  selectizeInput("select_white",
                                                                                 "White",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3, 
                                                                  selectizeInput("select_multi",
                                                                                 "Multiracial Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3, 
                                                                  selectizeInput("select_notlisted",
                                                                                 "Another Racial Category Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           )
                                                         ),
                                                         h3("Gender Variables"),
                                                         fluidRow(
                                                           column(width=3, 
                                                                  selectizeInput("select_gender_column",
                                                                                 "Gender Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=3, 
                                                                  selectizeInput("select_woman",
                                                                                 "Woman Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           ),
                                                           column(width=3, 
                                                                  selectizeInput("select_man",
                                                                                 "Man Code",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Code"))
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  h3("Charge Variables")),
                                                           column(width=4,
                                                                  h3("Arrest")),
                                                           column(width=4,
                                                                  h3("Type of Arrest"))
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectizeInput("select_charges",
                                                                                 "Select Charge Column(s)",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = TRUE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_arrest",
                                                                                 "Arrest Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_arrestTypes",
                                                                                 "Select Arrest Type(s)",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = TRUE,
                                                                                 options = list(placeholder = "Select Codes"))
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  h3("Bond Amount")),
                                                           column(width=4,
                                                                  h3("Patrol Unit")),
                                                           column(width=4,
                                                                  h3("Arresting Officer"))
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectizeInput("select_bond",
                                                                                 "Bond Amount Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_patrol",
                                                                                 "Patrol Unit Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_arrestingofficer",
                                                                                 "Arresting Officer Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           )
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=4,
                                                                  h3("Use of Force")),
                                                           column(width=4,
                                                                  h3("Search Conducted")),
                                                           column(width=4,
                                                                  h3("Stopped"))
                                                         ),
                                                         fluidRow(
                                                           column(width=4,
                                                                  selectizeInput("select_useofforce",
                                                                                 "Use of Force Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_searchconducted",
                                                                                 "Search Conducted Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("select_stopped",
                                                                                 "Stopped Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           )
                                                         ),
                                                         

                                                      
                                                         h3("Date and Time"),
                                                         fluidRow(
                                                           column(width=3,
                                                                  selectizeInput("select_date",
                                                                                 "Date Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Column"))
                                                           ),
                                                           column(width=3,
                                                                  selectizeInput("select_timezone",
                                                                                 "Time Zone Column",
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE,
                                                                                 options = list(placeholder = "Select a Time Zone"))
                                                           )
                                                         ),
                                                         h3("Census API Input"),
                                                         fluidRow(
                                                           column(width=4, 
                                                                  textInput("census_api_key", label = " API Key",  
                                                                            value = "985901667535f61f5ea97bfbf8e4fdfcd8c743c4",
                                                                            width = "400px")),
                                                           column(width=4, 
                                                                  selectizeInput("census_year", label = "Year", choices = 2017:2021, selected = 2021,
                                                                                 multiple = FALSE, 
                                                                                 options = list(placeholder = "Select a Year"))
                                                           ),
                                                           column(width=4,
                                                                  selectizeInput("geolevel", label = "Geography", choices = "tract", selected = "tract"), multiple=FALSE)
                                                         ),
                                                         h3("Geographic Input"),
                                                         fluidRow(
                                                           column(width=4,selectizeInput("state", label = "State", 
                                                                                         choices = c("", "AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                                                                                                     "DE", "DC", "FL", "GA", "HI", "ID", "IL",
                                                                                                     "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                                                                                     "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                                                                                                     "NY","NC", "ND", "OH", "OK", "OR", "PA",
                                                                                                     "PR", "RI", "SC", "SD", "TN", "TX", "UT",
                                                                                                     "VT", "VA", "VI", "WA", "WV", "WI", "WY"),
                                                                                         selected = "",
                                                                                         multiple = FALSE,
                                                                                         options = list(placeholder = "Select a State"))
                                                           ),
                                                           column(width=4, 
                                                                  selectizeInput("county", label = "County", 
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple=FALSE,
                                                                                 options = list(placeholder = "Select a County"))
                                                                  ),
                                                           column(width=4, #https://www2.census.gov/geo/docs/reference/codes/PLACElist.txt
                                                                  selectizeInput("municipality", label = " Municipality", 
                                                                                 choices = NULL,
                                                                                 selected = NULL,
                                                                                 multiple = FALSE ,
                                                                                 options = list(placeholder = "Municipality"))
                                                           )
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=3, actionButton("completeAnalysis", "Complete Analysis")),
                                                           column(width=3, downloadButton("downloadresultsZip", "Download Results"))
                                                         ), br(), br(), br(), br(), br(), br(), br(), br()
                                                       )
                                              ),
                                              
                                              #display dataset
                                              tabPanel("Data Preview", tags$hr(), value="data",
                                                       verbatimTextOutput("warning"),
                                                       br(),
                                                       DT::dataTableOutput("preview.data")),
                                              
                                              
                                              
                                              tabPanel("Q1", tags$hr(), value="Q1",
                                                       fluidPage(
                                                         h1("What proportion of all police incidents go to different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q1_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q1_height", "Height", value=7)),
                                                           column(width=2, textInput("Q1_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q1_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q1_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q1_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q1_tab")), style="margin-bottom: 30px;"),
                                                       br(),
                                                       h1("Interpretation"), 
                                                       uiOutput("Q1_interp", align="left", style="margin-bottom: 30px;")
                                              ),
                                              
                                              tabPanel("Q2", tags$hr(), value="Q2",
                                                       fluidPage(
                                                         h1("What proportion of incidents related to routine traffic/moving violations go to different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q2_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q2_height", "Height", value=7)),
                                                           column(width=2, textInput("Q2_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q2_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q2_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q2_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q2_tab")), style="margin-bottom: 30px;"),
                                                       br(),
                                                       h1("Interpretation"), 
                                                       uiOutput("Q2_interp", align="left", style="margin-bottom: 30px;")
                                              ),
                                              
                                              tabPanel("Q3", tags$hr(), value="Q3",
                                                       fluidPage(
                                                         h1("What proportion of incidents related to drug and/or firearm possession go to different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q3_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q3_height", "Height", value=7)),
                                                           column(width=2, textInput("Q3_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q3_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q3_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q3_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q3_tab")), style="margin-bottom: 30px;"),
                                                       br(),
                                                       h1("Interpretation"), 
                                                       uiOutput("Q3_interp", align="left", style="margin-bottom: 30px;")
                                              ),
                                              
                                              tabPanel("Q4", tags$hr(), value="Q4",
                                                       fluidPage(
                                                         h1("What proportion of incidents related to quality of life violations go to different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q4_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q4_height", "Height", value=7)),
                                                           column(width=2, textInput("Q4_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q4_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q4_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q4_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q4_tab")), style="margin-bottom: 30px;"),
                                                       br(),
                                                       h1("Interpretation"), 
                                                       uiOutput("Q4_interp", align="left", style="margin-bottom: 30px;")
                                              ),
                                              
                                              tabPanel("Q5", tags$hr(), value="Q5",
                                                       fluidPage(
                                                         h1("What proportion of incidents that resulted in an arrest go to different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q5_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q5_height", "Height", value=7)),
                                                           column(width=2, textInput("Q5_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q5_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q5_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q5_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q5_tab")), style="margin-bottom: 30px;"),
                                                       br(),
                                                       h1("Interpretation"), 
                                                       uiOutput("Q5_interp", align="left", style="margin-bottom: 30px;")
                                              ),
                                              
                                              tabPanel("Q6", tags$hr(), value="Q6",
                                                       fluidPage(
                                                         h1("What proportion of incidents that occured resulted in an arrest for different gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q6_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q6_height", "Height", value=7)),
                                                           column(width=2, textInput("Q6_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q6_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q6_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q6_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q6_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q7", tags$hr(), value="Q7",
                                                       fluidPage(
                                                         h1("How does average bond amount differ according to race and gender?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q7_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q7_height", "Height", value=7)),
                                                           column(width=2, textInput("Q7_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q7_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q7_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q7_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q7_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q8", tags$hr(), value="Q8",
                                                       fluidPage(
                                                         h1("What proportion of police incidents go to different race groups within each patrolling beat/neighborhood?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q8_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q8_height", "Height", value=7)),
                                                           column(width=2, textInput("Q8_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q8_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q8_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q8_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q8_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q9", tags$hr(), value="Q9",
                                                       fluidPage(
                                                         h1("What proportion of police incidents go to different race groups for each of the most active officers?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q9_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q9_height", "Height", value=7)),
                                                           column(width=2, textInput("Q9_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q9_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q9_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q9_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q9_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q10", tags$hr(), value="Q10",
                                                       fluidPage(
                                                         h1("What number of police incidents occur during each weekday/hour of day?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q10_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q10_height", "Height", value=7)),
                                                           column(width=2, textInput("Q10_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q10_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q10_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q10_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q10_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q11", tags$hr(), value="Q11",
                                                       fluidPage(
                                                         h1("Are officers more likely to use force on subjects from specific gender/race groups?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q11_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q11_height", "Height", value=7)),
                                                           column(width=2, textInput("Q11_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q11_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q11_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q11_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q11_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q12", tags$hr(), value="Q12",
                                                       fluidPage(
                                                         h1("Are subjects from specific gender/race groups stopped in street checks at higher rates?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q12_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q12_height", "Height", value=7)),
                                                           column(width=2, textInput("Q12_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q12_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q12_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q12_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q12_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                              tabPanel("Q13", tags$hr(), value="Q13",
                                                       fluidPage(
                                                         h1("Are subjects from specific gender/race groups more likely to be searched if they are stopped in a street check?"),
                                                         fluidRow(shinycssloaders::withSpinner(plotOutput("Q13_plot"))),
                                                         fluidRow(
                                                           column(width=2, textInput("Q13_height", "Height", value=7)),
                                                           column(width=2, textInput("Q13_width", "Width", value=7)),
                                                           column(width=2, selectInput("Q13_unit", "Units", choices = c("in", "cm"))),
                                                           column(width=2, selectInput("Q13_format", "Format", choices = c("png", "pdf", "tiff", "bmp"))),
                                                           column(width=2, downloadButton('Q13_downloadPlot'),style = "margin-top: 25px;"), #
                                                           tags$head(tags$style(HTML(".selectize-input {height: 42px;}")))
                                                         ),
                                                         br(),
                                                         h1("Numerical Summary"),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("Q13_tab")), style="margin-bottom: 30px;"),
                                              ),
                                              
                                  ))
                       ),
                       tabPanel("References",
                                fluidPage(
                                  includeHTML("www/livebib.html"), br(), br(), br(), br(), br()
                                )
                       )
            )
)


