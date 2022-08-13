---
title: "Getting started with shiny"
slug: "getting-started-with-shiny"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Simple App
Each `shiny` app contains two parts: A user interface definition (`UI`) and a server script (`server`). This example shows how you can print "Hello world" from UI or from server.

**UI.R**

In the UI you can place some view objects (div, inputs, buttons, etc).

    library(shiny)
    
    # Define UI for application print "Hello world" 
    shinyUI(

      # Create bootstrap page 
      fluidPage(
        
        # Paragraph "Hello world"
        p("Hello world"),

        # Create button to print "Hello world" from server
        actionButton(inputId = "Print_Hello", label = "Print_Hello World"),

        # Create position for server side text
        textOutput("Server_Hello")
        
      )
    )

**Server.R**

In the server script you can define methods which manipulate data or listen to actions.

    # Define server logic required to print "Hello World" when button is clicked
    shinyServer(function(input, output) {
      
      # Create action when actionButton is clicked
      observeEvent(input$Print_Hello,{

        # Change text of Server_Hello
        output$Server_Hello = renderText("Hello world from server side")
      })
      
      
    })

**How to run?**

You can run your app in several ways:

 1. Create two different files and place them into one directory, then use `runApp('your dir path')`
 2. You can define two variables (ui and server, for example) and then use `shinyApp(ui,server)` to run your app


**Result**

In this example you will see some text and a button:

[![screenshot before click][1]][1]

And after button click the server responds:

[![screenshot after click][2]][2]


  [1]: http://i.stack.imgur.com/hbvN5.png
  [2]: http://i.stack.imgur.com/hqx28.png

## Installation or Setup
Shiny can run as a standalone application on your local computer, on a server that can provide shiny apps to multiple users (using shiny server), or on [shinyapps.io][1].

1. **Installing Shiny on a local computer:** in R/RStudio, run `install.packages("shiny")` if installing from CRAN, or `devtools::install_github("rstudio/shiny")` if installing from the RStudio Github repository. The Github repository hosts a development version of Shiny which can possibly have more features when compared to the CRAN version, but it may also be unstable.


  [1]: http://shinyapps.io

## When would I use shiny?
 1. I have some data analysis done on some data and have many
    'non-coding' people on the team, who have similar data like mine,
    and have similar analysis requirements. In such cases, I can build a
    web application with shiny, which takes in user specific input data
    files, and generate analyses.
 2. I need to share analyzed data or relevant plots with others in the team. Shiny web apps can be useful in such situations.
 3. I don't have significant experience with web application programming, but need to quickly assemble a simple interface. Shiny to the rescue with easy UI and server elements and minimum coding.
 4. Interactive elements allow your users to explore what element of the data is relevant to them. For example, you could have data for the whole company loaded, but have a dropdown per department like "Sales", "Production", "Finance" that can summarise the data the way the users want to view it. The alternative would be producing a huge report pack with analyses for each department, but they only read their chapter and the total.  


## Including plots
The simplest way to include plots in your shinyApp is to use `plotOutput` in the ui and `renderPlot` in the server. This will work with base graphics as well as `ggPlot`s

    library(shiny)
    library(ggplot2)

    ui <- fluidPage(
      plotOutput('myPlot'),
      plotOutput('myGgPlot')
    )

    server <- function(input, output, session){
      output$myPlot = renderPlot({
        hist(rnorm(1000))
      })
      output$myGgPlot <- renderPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
      })
    }

    shinyApp(ui, server)

## Including tables
Tables are most easily included with the [DT package][1], which is an R interface to the JavaScript library DataTables. 

    library(shiny)
    library(DT)
    
    ui <- fluidPage(
      dataTableOutput('myTable')
    )
    
    server <- function(input, output, session){
      output$myTable <- renderDataTable({
        datatable(iris)
      })
    }
    
    shinyApp(ui, server)


  [1]: https://rstudio.github.io/DT/

