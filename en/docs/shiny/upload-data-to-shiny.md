---
title: "Upload Data to shiny"
slug: "upload-data-to-shiny"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Uploading csv files to Shiny
It is also possible to have an user upload csv's to your Shiny app. The code below shows a small example on how this can be achieved. It also includes a radioButton input so the user can interactively choose the separator to be used.

    library(shiny)
    library(DT)
    
    # Define UI
    ui <- shinyUI(fluidPage(
      
      fileInput('target_upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
      DT::dataTableOutput("sample_table")
    )
    )
    
    # Define server logic
    server <- shinyServer(function(input, output) {
      
      df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
          return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
        return(df)
      })
      
      output$sample_table<- DT::renderDataTable({
        df <- df_products_upload()
        DT::datatable(df)
      })
      
    }
    )
    
    # Run the application 
    shinyApp(ui = ui, server = server)

## Upload .RData Files to shiny with fileInput()
The example allows you to upload .RData files. The approach with `load` and `get` allows you to assign the loaded data to a variable name of your choice. For the matter of the example being "standalone" I inserted the top section that stores two vectors to your disk in order to load and plot them later.

    library(shiny)
    
    # Define two datasets and store them to disk
    x <- rnorm(100)
    save(x, file = "x.RData")
    rm(x)
    y <- rnorm(100, mean = 2)
    save(y, file = "y.RData")
    rm(y)
    
    # Define UI
    ui <- shinyUI(fluidPage(
      titlePanel(".RData File Upload Test"),
      mainPanel(
        fileInput("file", label = ""),
        actionButton(inputId="plot","Plot"),
        plotOutput("hist"))
      )
    )
    
    # Define server logic
    server <- shinyServer(function(input, output) {

      observeEvent(input$plot,{
        if ( is.null(input$file)) return(NULL)
        inFile <- input$file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        data <- e[[name]]
        
        # Plot the data
        output$hist <- renderPlot({
          hist(data)
        })
      })
    })
    
    # Run the application 
    shinyApp(ui = ui, server = server)

