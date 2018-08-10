##################################################
# A Shiny app for creating custom IPEDS peer     #
# reports.                                       #
##################################################

## Load packages -----
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
library(rmarkdown)
library(tidyr)
library(openxlsx)
library(data.table)
library(magrittr)
library(reshape2)
library(RODBC)

## Load sources -----
# source("load_data.R")
source("functions.R")
# source("queries.R")

## Beginning of server -----
shinyServer(function(input, output, session) {

  ## Read .csv file
  filedata <- reactive({
    infile <- input$control_file
    if (is.null(infile)) {
      return(NULL)
    }
    df_subset_options <- read.csv(infile$datapath)
  })
  
  ## Subsetting options
  datasetInput <- reactive({
    # df <- df[df$State %in% input$control_file, ]
  })
  
  ## Beginning of main body -----
  output$mainbody <- renderUI({
    fluidPage(
      
      theme = "mystyle.css",
      br(), br(),
      titlePanel("IPEDS Peer Reports"), br(),
      h3("Supported by the University of Nevada, Las Vegas"), br(),
      br(), br(),
      br(), br(), 
      
      ## Sidebar and Subsetting Options -----
      sidebarLayout(
        sidebarPanel(
          ## Subsetting options 
          h4("Upload"), br(),
          fileInput(inputId = "control_file", 
                    label = h6("Choose file to subset the data"),
                    accept = c("text/csv", 
                               "text/comma-separated-values,text/plain")
                    ),
          hr(),
          br(),
          
          ## Download options for subsetted data 
          h4("Download Results"), br(),
          div(style = "display: inline-block;", 
              downloadButton("download_data", 
                              h5("Data")
                            )
              ),
          div(style = "display: inline-block;", 
              downloadButton("download_report",                      
                              h5("Report")
                             )
              ),
          br(), br()
        ), 
        ## End of side bar
        
        ## Main Panel -----
        mainPanel(
          ## View the subsetted options into two tabs - Table and Preview report
          tabsetPanel(type = "tabs",
                      tabPanel("Data table", class = "one",      ## Table
                               dataTableOutput("table")
                               ),
                      tabPanel("Preview Report", class = "one",  ## Report preview
                               uiOutput("report")
                               )
                      ) ## End of Tabset Panel
                  ) ## End of Main panel
              ) ## End of Sidebar Layout
          ) ## End of Fluid Page
      }) ## End of Main body
  
  ## Data Table Tab -----
  output$table <- renderDataTable({
        datatable(datasetInput(), 
                  select = "none",
                  options = list(lengthMenu = c(5, 10, 25, 50, 100), ## lengthMenu is used for selecting the amount of rows of data to show
                                 pageLength = 5),                    ## pageLength is the default length, currently 5
                  rownames = FALSE)
  })
  
  
  ## Report Tab -----
  output$report <- renderUI({
    tagList(
      ## Suppress warning messages  
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
                )
          )
  })
  

  ## Download report as PDF (rmarkdown) -----
  output$download_report <- downloadHandler(
    filename = "custom_report.pdf",
    content = function(file) {
      out <- render("download_report.Rmd", pdf_document())
      file.rename(out, file)
    }
  )
  
  ## Download raw, subsetted data as a .csv -----
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
})

## End of server -----
