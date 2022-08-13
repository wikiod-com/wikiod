---
title: "Javascript API"
slug: "javascript-api"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- session$sendCustomMessage(*name*,*list of parameters*)
- Shiny.addCustomMessageHandler(*name*, *JS function that accepts list of parameters*)
- Shiny.onInputChange(*name*,*value*)

## Sending data from server to client
In many instances, you will want to send data from the R server to the JS client. Here is a very simple example:

    library(shiny)
    runApp(
      list(
        ui = fluidPage(
          tags$script(
            "Shiny.addCustomMessageHandler('message', function(params) { alert(params); });"  
          ),
          actionButton("btn","Press Me")
        ),
        server = function(input, output, session) {
          observeEvent(input$btn,{
            randomNumber <- runif(1,0,100)
            session$sendCustomMessage("message",list(paste0(randomNumber," is a random number!")))
          })
        }
      )
    )
The workhorses here are the `session$sendCustomMessage` function in `R` and the `Shiny.addCustomMessageHandler` function in `javascript`.

The `session$sendCustomMessage` function lets you send parameters from `R` to a `javascript` function, and `Shiny.addCustomMessageHandler` let's you define the `javascript` function that accepts the parameters from `R`. 

Note: Lists are converted to JSON when they are passed from `R` to `javascript`

## Sending data from client to server
In some instances, you will want to send data from JS client to the R server. Here is a basic example using javascript's `Shiny.onInputChange` function:

    library(shiny)
    runApp(
      list(
        ui = fluidPage(
          # create password input
          HTML('<input type="password" id="passwordInput">'),
          # use jquery to write function that sends value to
          # server when changed
          tags$script(
            '$("#passwordInput").on("change",function() {
              Shiny.onInputChange("myInput",this.value);
            })'
          ),
          # show password
          verbatimTextOutput("test")
        ),
        server = function(input, output, session) {
          # read in then show password
          output$test <- renderPrint(
            input$myInput
          )
        }
      )
    )
Here we create a password input with id `passwordInput`. We add a Javascript function on the UI that reacts to changes in `passwordInput`, and sends the value to the server using `Shiny.onInputChange`.

`Shiny.onInputChange` takes two parameters, a name for the `input$*name*`, plus a value for `input$*name*`

Then you can use `input$*name*` like any other Shiny input.

