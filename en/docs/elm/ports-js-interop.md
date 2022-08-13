---
title: "Ports (JS interop)"
slug: "ports-js-interop"
draft: false
images: []
weight: 9638
type: docs
toc: true
---

## Syntax
- Elm (receiving): port functionName : (value -> msg) -> Sub msg
- JS (sending): app.ports.functionName.send(value)
- Elm (sending): port functionName : args -> Cmd msg
- JS (receiving): app.ports.functionName.subscribe(function(args) { ... });

Consult http://guide.elm-lang.org/interop/javascript.html from *The Elm Guide* to aid in understanding these examples.

## Outgoing
Outgoing ports are used as Commands, that you return from your `update` function.

# Elm side #

Define outgoing port:

    port output : () -> Cmd msg

In this example we send an empty Tuple, just to trigger a subscription on the JavaScript side.

To do so, we have to apply `output` function with an empty Tuple as argument, to get a Command for sending the outgoing data from Elm.

    update msg model =
        case msg of
            TriggerOutgoing data ->
                ( model, output () )

# JavaScript side #
Initialize the application:

    var root = document.body;
    var app = Elm.Main.embed(root);

Subscribe to a port with a corresponding name:

    app.ports.output.subscribe(function () {
        alert('Outgoing message from Elm!');
    });

# Note #
As of `0.17.0`, immediate outgoing message to JavaScript from your `initial` state will have no effect.

    init : ( Model, Cmd Msg )
    init =
        ( Model 0, output () ) -- Nothing will happen

See the workaround in the example below.



## Incoming
Incoming data from JavaScript is going through Subscriptions.

# Elm side #
First, we need to define an incoming port, using the following syntax:

    port input : (Int -> msg) -> Sub msg

We can use `Sub.batch` if we have multiple subscriptions, this example will only contain one Subscription to `input port`

    subscriptions : Model -> Sub Msg
    subscriptions model =
        input Get

Then you have to pass the `subscriptions` to your `Html.program`:

    main =
        Html.program
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

# JavaScript side #
Initialize the application:

    var root = document.body;
    var app = Elm.Main.embed(root);

Send the message to Elm:

    var counter = 0;
            
    document.body.addEventListener('click', function () {
        counter++;
        app.ports.input.send(counter);
    });

# Note #
Please note, that as of `0.17.0` the immediate `app.ports.input.send(counter);` after app initialization will have no effect!

Pass all the required data for the start-up as Flags using `Html.programWithFlags`

## Get started
**index.html**

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8">
        <title>Trying out ports</title>
      </head>
      <body>
        <div id="app"></div>
        <script src="elm.js"></script>
        <script>
        
          var node = document.getElementById('app');
          var app = Elm.Main.embed(node);
          
          // subscribe to messages from Elm
          app.ports.toJs.subscribe(function(messageFromElm) {
            alert(messageFromElm);
            // we could send something back by
            // app.ports.fromJs.send('Hey, got your message! Sincerely, JS');
          });
          
          // wait three seconds and then send a message from JS to Elm
          setTimeout(function () {
            app.ports.fromJs.send('Hello from JS');
          }, 3000);
          
        </script>
      </body>
    </html>

**Main.elm**

<!-- language: lang-hs -->

    port module Main exposing (..)

    import Html

    port toJs : String -> Cmd msg
    port fromJs : (String -> msg) -> Sub msg

    main =
       Html.program
            { init = (Nothing, Cmd.none) -- our model will be the latest message from JS (or Nothing for 'no message yet')
            , update = update
            , view = view
            , subscriptions = subscriptions
            }

    type Msg
        = GotMessageFromJs String

    update msg model =
        case msg of
            GotMessageFromJs message ->
                (Just message, toJs "Hello from Elm")

    view model =
        case model of
            Nothing ->
                Html.text "No message from JS yet :("
            Just message ->
                Html.text ("Last message from JS: " ++ message)

    subscriptions model =
        fromJs GotMessageFromJs

Install the `elm-lang/html` package if you haven't yet by `elm-package install elm-lang/html --yes`.

Compile this code using `elm-make Main.elm --yes --output elm.js` so that the HTML file finds it.

If everything goes well, you should be able to open the `index.html` file with the "No message" text displayed. After three seconds the JS sends a message, Elm gets it, changes its model, sends a response, JS gets it and opens an alert.

## Immediate outgoing message on start-up in 0.17.0
To send an immediate message with data to JavaScript, you have to trigger an action from your `init`.

    init : ( Model, Cmd Msg )
    init =
        ( Model 0, send SendOutgoing )
    
    
    send : msg -> Cmd msg
    send msg =
        Task.perform identity identity (Task.succeed msg)

## Overview
A module, that is using Ports should have `port` keyword in it's module definition.

    port module Main exposing (..)

It is impossible to use ports with `Html.App.beginnerProgram`, since it does not allow using Subscriptions or Commands.

Ports are integrated in to update loop of `Html.App.program` or `Html.App.programWithFlags`.

# Note #
`program` and `programWithFlags` in elm 0.18 are inside the package `Html` instead of `Html.App`.

