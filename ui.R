#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(splitstackshape)
library(plotly)
#library(markdown)
library(leaflet)
library(htmltools)
library(htmlwidgets)

#spain <- read.delim("data/analysis_txtn_qc_segmented.txt")
spain <- readRDS("data/spain.rds")
estaspain <- read.delim("data/statistics.txt")
#estaspain <- read.delim("data/statistics.txt")
biz<-as.character(names(spain)[c(5,7,9,11,13,15,17,19,21,22,23,24,25,26)])
biz2<-as.character(names(spain)[c(17,19,23,26)])
biz3<-as.character(names(estaspain)[c(4:83)])

# Defines UI 
shinyUI ( 
    navbarPage(
        "IMPACTRON DATASET",

        ############################
        #Scatter plot
        ############################
        
        tabPanel("Scatter plot",helpText("Plotting App for the IMPACTRON dataset"),
          verticalLayout(
            column(6,
            wellPanel(
             fluidRow(
              column(5,
                selectInput("stationscatter", 
                        label = "Choose a station to display",
                        choices =as.character(levels(spain$code))
                )
              ),
              column(5,offset = 1,
                    selectInput("var1", 
                        label = "Choose a variable to display",
                        choices = biz,
                        selected='dtx')
               )
               
              )
            )
            ),
                plotOutput("scatter"),
                column(9,
                  br(),
                  column(3,
                  uiOutput("title")
                  ),
                  column(7,
                  uiOutput("plots")
                  )
                  
                )
          )
          ),
       
    ############################
    #Box plot
    ############################
    
    tabPanel("Box plot",helpText("Plotting App for the IMPACTRON dataset"),
             verticalLayout(
               column(9,
               wellPanel(
                 #helpText("Plotting App for the IMPACTRON dataset"),
                 fluidRow(
                   column(3,
                      selectInput("station", 
                          label = "Choose a station to display",
                          choices =as.character(levels(spain$code))
                      )
                    ),
                    column(3, offset = 1,
                        selectInput("stratos", 
                                      label = "Show by",
                                      choices = c("Season", "Month","Segment"),
                                      selected = "Month")
                     ),
                     column(3,offset = 1,
                        # Outputs the dynamic UI component
                        uiOutput("ui")
                     )  
                  )
               )
               ),

               # Show plot and summary 
               
               
                  plotlyOutput("boxplot", height = 600),
                  br(),br()

               )
             ),
    
    ############################
    #All stations
    ############################
    
    tabPanel("All stations",helpText("Plotting App for the IMPACTRON dataset"),
             verticalLayout(
               column(3,
               wellPanel(
                 fluidRow(
                   
                    selectInput("var", 
                             label = "Choose a variable to display",
                             choices = biz3,
                             selected='median_dtx')
                 
                 # Outputs the dynamic UI component
                   
                 )
               )
               ),
                 plotlyOutput("scatterx"),
                 br(),br(),
                 verbatimTextOutput("resumenx")
             )),
    
    ############################
    #Summaries
    ############################
    tabPanel("Summaries",helpText("Plotting App for the IMPACTRON dataset"),
             verticalLayout(
               column(7,
                      wellPanel(
                        fluidRow(
                          column(3,
                                 selectInput("stationsDT", 
                                             label = "Choose a station to display",
                                             choices =as.character(levels(estaspain$code))
                                 )
                          )
                          
                        )
                      )
                      
               )
             ),
             column(9,
                    br(),
                    column(9,
                           DT::dataTableOutput("mytable"))
                    )
                    
             ),
            
    ############################
    #Map
    ############################
    tabPanel("Map",helpText("Plotting App for the IMPACTRON dataset"),
             div(class="outer",
                 
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css"),
                   includeScript("gomap.js")
                 ),
                 leafletOutput("MapPlot1", width="100%", height="100%"),
                 
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = 45, right = "auto", bottom = "auto",
                               width = 450, height = "auto",
                               
                               h2("Station explorer"),
                               
                               selectInput("var2", 
                                           label = "Choose a variable to display",
                                           choices = biz2,
                                           selected='dtx'),
                               br(),br(),br(),br(),br(),
                               plotlyOutput("plot",width = "420px",height="420px")
                 ),
                 tags$div(id="cite",
                          'Data from ', tags$em('Impactron project')
                 )
                 
             )),
    
    ############################
    #Info
    ############################
    
    
    tabPanel("Info",
             actionButton(inputId='ab1', label="Get code", 
                          icon = icon("folder-open"), 
                          onclick ="window.open('https://github.com/kairocafe/IMPACTRON/tree/beta1')")
    )
  )

)
