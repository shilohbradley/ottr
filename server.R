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

## Load sources -----


## Beginning of server -----
shinyServer(function(input, output, session) {
  
  ## Beginning of main body -----
  output$mainbody <- renderUI({
    ## Beginning of fluid page
    fluidPage(
      
      # theme = "mystyle.css",
      br(), br(),
      titlePanel("IPEDS Peer Reports"),
      br(), br(),
      
      ## Sidebar and Subsetting Options -----
      sidebarLayout(
        sidebarPanel(
          h3("Please select data: "),
          br(), hr(),
          ## Subsetting options 
          
          hr(),
          ## Download options for subsetted data -----
          downloadButton("downloadData", h5("Download Data")),
          downloadButton("downloadReport", h5("Download Report")),
          br(), br()
        ), 
        ## End of side bar
        
        ## Main Panel -----
        mainPanel(
          ## View the subsetted options into two tabs - Table and Preview report
          tabsetPanel(type = "tabs",
                      ## Table
                      tabPanel("Data table", class = "one",
                               div(HTML("<br><h2><b><center>IPEDS</center></b></h2></br>")),
                               DT::dataTableOutput("table")),
                      ## Report preview
                      tabPanel("Preview Report", class = "one",
                               div(HTML("<br><h2><b><center>IPEDS</center></b></h2></br>")),
                               uiOutput("report"))
          ) ## End of Tabset Panel
        ) ## End of Main panel
      ) ## End of Sidebar Layout
    ) ## End of Fluid Page
  }) ## End of Main body
  
  ## Data Table Tab -----
  output$table <- DT::renderDataTable({
    DTpreview <- PreviewDT(datasetInput(), cnm)
    DT::datatable(DTpreview, select = "none",
                  options = list(lengthMenu = c(5, 10, 25, 50, 100), 
                                 ## lengthMenu is used for selecting the amount of rows of data to show
                                 pageLength = 5),
                  ## pageLength is the default length, currently 5
                  rownames = FALSE)
  })
  
  
  ## Report Tab -----
  output$report <- renderUI({
    tagList(
      ## Suppress warning messages  
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
                )
          )
  })
  
  output$plot <- renderPlot({
    if(nrow(datasetInput()) > 1 && length(input$questions) >= 1) {
      testQ(datasetInput())
    }
  })
  

  ## Download report as PDF (rmarkdown) -----
  output$downloadReport <- downloadHandler(
    filename = "myreportpdf.pdf",
    content = function(file) {
      out <- render("download_report.Rmd", pdf_document())
      file.rename(out, file)
    }
  )
  
  ## Download raw, subsetted data as a .csv -----
  output$downloadData <- downloadHandler(
    filename = "mydownload.csv",
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
})

## End of server -----
