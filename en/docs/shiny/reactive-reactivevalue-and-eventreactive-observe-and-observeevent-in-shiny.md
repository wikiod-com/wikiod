---
title: "reactive, reactiveValue and eventReactive, observe and observeEvent in Shiny"
slug: "reactive-reactivevalue-and-eventreactive-observe-and-observeevent-in-shiny"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

**reactive, reactiveValue and eventReactive** are various kinds of reactive expressions in Shiny. They yield output which can be used as input in other expressions, which will in turn take a dependency on the reactive expression.

**observe and observeEvent** are similar to reactive expressions. The big difference is that the observers do not yield any output and thus they are only useful for their side effects.

Examples of their use are given in this document.

## reactive
A reactive can be used to make output depend on another expression. In the example below, the output$text element is dependent on text_reactive, which in turn is dependent on input$user_text. Whenever input$user_text changes, output$text element and text_reactive become **invalidated.** They are recalculated based on the new value for input$user_text.

    library(shiny)
    
    ui <- fluidPage(
      headerPanel("Example reactive"),
      
      mainPanel(
        
        # input field
        textInput("user_text", label = "Enter some text:", placeholder = "Please enter some text."),
        
        # display text output
        textOutput("text"))
    )
    
    server <- function(input, output) {
      
      # reactive expression
      text_reactive <- reactive({
        input$user_text
      })
      
      # text output
      output$text <- renderText({
        text_reactive()
      })
    }
    
    shinyApp(ui = ui, server = server)

## eventReactive
eventReactives are similar to reactives, they are constructed as follows:

    eventReactive( event {
    code to run
    })

eventReactives are not dependent on all reactive expressions in their body ('*code to run*' in the snippet above). Instead, they are only dependent on the expressions specified in the *event* section. 

In the example below, we have added a submit button, and created an eventReactive. Whenever input$user_text changes, the eventReactive is not invalidated, since the eventReactive is only dependent on the actionButton input$submit. Whenever that button is pressed, text_reactive and subsequently output$text are invalidated, and will be recalulated based on the updated input$user_text.
    
    library(shiny)
    
    ui <- fluidPage(
      headerPanel("Example eventReactive"),
      
      mainPanel(
        
        # input field
        textInput("user_text", label = "Enter some text:", placeholder = "Please enter some text."),

        # submit button
        actionButton("submit", label = "Submit"),
        
        # display text output
        textOutput("text"))
    )
    
    server <- function(input, output) {
      
      # reactive expression
      text_reactive <- eventReactive( input$submit, {
        input$user_text
      })
      
      # text output
      output$text <- renderText({
        text_reactive()
      })
    }
    
    shinyApp(ui = ui, server = server)

## reactiveValues
reactiveValues can be used to store objects, to which other expressions can take a dependency. 

In the example below, a reactiveValues object is initialized with value "No text has been submitted yet.". A separate observer is created to update the reactiveValues object whenever the submit button is pressed. Note that the reactiveValues itself does not take a dependency on the expressions in its body.  
   
    library(shiny)
    
    ui <- fluidPage(
      headerPanel("Example reactiveValues"),
      
      mainPanel(
        
        # input field
        textInput("user_text", label = "Enter some text:", placeholder = "Please enter some text."),
        actionButton("submit", label = "Submit"),
        
        # display text output
        textOutput("text"))
    )
    
    server <- function(input, output) {
      
      # observe event for updating the reactiveValues
      observeEvent(input$submit,
                   {
        text_reactive$text <- input$user_text
      })
      
      # reactiveValues
      text_reactive <- reactiveValues(
        text = "No text has been submitted yet."
      )
      
      # text output
      output$text <- renderText({
        text_reactive$text
      })
    }
    
    shinyApp(ui = ui, server = server)

## observeEvent
An observeEvent object can be used to trigger a piece of code when a certain event occurs. It is constructed as:

    observeEvent( event {
    code to run
    })

The observeEvent will only be dependent on the *'event'* section in the small piece of code above. It will not be dependent on anything in the '*code to run*' part. An example implementation can be found below:

    library(shiny)
    
    ui <- fluidPage(
      headerPanel("Example reactive"),
      
      mainPanel(
        
        # action buttons
        actionButton("button1","Button 1"),
        actionButton("button2","Button 2")
      )
    )
    
    server <- function(input, output) {
      
      # observe button 1 press.
      observeEvent(input$button1, {
        # The observeEvent takes no dependency on button 2, even though we refer to the input in the following line.
        input$button2  
        showModal(modalDialog(
          title = "Button pressed",
          "You pressed one of the buttons!"
        ))
      })
    }
    
    shinyApp(ui = ui, server = server)

## observe
An observe expression is triggered every time one of its inputs changes. The major difference with regards to a reactive expression is that it yields no output, and it should only be used for its side effects (such as modifying a reactiveValues object, or triggering a pop-up).

Also, note that observe does not ignore NULL's, therefore it will fire even if its inputs are still NULL. observeEvent by default does ignore NULL, as is almost always desirable.

    library(shiny)
    
    ui <- fluidPage(
      headerPanel("Example reactive"),
      
      mainPanel(
        
        # action buttons
        actionButton("button1","Button 1"),
        actionButton("button2","Button 2")
      )
    )
    
    server <- function(input, output) {
      
      # observe button 1 press.
      observe({
        input$button1
        input$button2  
        showModal(modalDialog(
          title = "Button pressed",
          "You pressed one of the buttons!"
        ))
      })
    }
    
    shinyApp(ui = ui, server = server)

