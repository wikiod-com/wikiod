---
title: "The Elm Architecture"
slug: "the-elm-architecture"
draft: false
images: []
weight: 9701
type: docs
toc: true
---

The recommended way to structure your applications is dubbed 'the Elm Architecture.' 

The simplest program consists of a `model` record storing all data that might be updated, a union type `Msg` that defines ways your program updates that data, a function `update` which takes the model and a `Msg` and returns a new model, and a function `view` which takes a model and returns the HTML your page will display. Anytime a function returns a `Msg`, the Elm runtime uses it to update the page.

## Program with Flags
`programWithFlags` has only one difference from `program`.

It can accept the data upon initialization from JavaScript:

    var root = document.body;
    var user = { id: 1, name: "Bob" };
    var app = Elm.Main.embed( root, user );

The data, passed from JavaScript is called Flags.

In this example we are passing a JavaScript Object to Elm with user information, it is a good practice to specify a Type Alias for flags.

    type alias Flags =
        { id: Int
        , name: String
        }

Flags are passed to the `init` function, producing the initial state:

    init : Flags -> ( Model, Cmd Msg )
    init flags =
        let
            { id, name } =
                flags
        in
            ( Model id name, Cmd.none )

You might notice the difference from it's type signature:

    programWithFlags :
        { init : flags -> ( model, Cmd msg )          -- init now accepts flags
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        }
        -> Program flags

The initialization code looks almost the same, since it's only `init` function that is different.

    main =
        programWithFlags
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            }


## Beginner program
[Html][1] has `beginnerProgram` mostly for learning purposes.

`beginnerProgram` is not capable of handling Subscriptions or running Commands.

It is only capable of handling user input from DOM Events.

It only requires a `view` to render the `model` and an `update` function to handle state changes.

# Example #
Consider this minimal example of `beginnerProgram`.

The `model` in this example consists of single `Int` value.

The `update` function has only one branch, which increments the `Int`, stored in the `model`.

The `view` renders the model and attaches click DOM Event.

See how to build the example in [Initialize and build][2]
   
<!-- language: lang-hs -->
    import Html exposing (Html, button, text)
    import Html exposing (beginnerProgram)
    import Html.Events exposing (onClick)
    
    
    main : Program Never
    main =
        beginnerProgram { model = 0, view = view, update = update }


    -- UPDATE


    type Msg
        = Increment

    update : Msg -> Int -> Int
    update msg model =
        case msg of
            Increment ->
                model + 1


    -- VIEW


    view : Int -> Html Msg
    view model =
        button [ onClick Increment ] [ text ("Increment: " ++ (toString model)) ]



  [1]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html
  [2]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build

## Program
`program` is a good pick, when your application does not require any external data for initialization.

It is capable of handling Subscriptions and Commands, which enables way more opportunities for handling I/O, such as HTTP communication or interop with JavaScript.

The initial state is required to return start-up Commands along with the Model.

The initialization of `program` will require `subscriptions` to be provided, along with `model`, `view` and `update`.

See the type definition:

    program :
        { init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        }
        -> Program Never

# Example
The simplest way to illustrate, how you can use [Subscriptions][2] is to setup a simple [Port][3] communication with JavaScript.

See how to build the example in [Initialize and build][1] / [Embedding into HTML][4]

<!-- language: lang-hs -->
    port module Main exposing (..)

    import Html exposing (Html, text)
    import Html exposing (program)


    main : Program Never
    main =
        program
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


    port input : (Int -> msg) -> Sub msg


    -- MODEL


    type alias Model =
        Int


    init : ( Model, Cmd msg )
    init =
        ( 0, Cmd.none )


    -- UPDATE


    type Msg = Incoming Int


    update : Msg -> Model -> ( Model, Cmd msg )
    update msg model =
        case msg of
            Incoming x ->
              ( x, Cmd.none )


    -- SUBSCRIPTIONS


    subscriptions : Model -> Sub Msg
    subscriptions model =
        input Incoming


    -- VIEW


    view : Model -> Html msg
    view model =
        text (toString model)


<!-- language: lang-html -->
    <!DOCTYPE html>
    <html>
        <head>
            <script src='elm.js'></script>
    </head>
        <body>
        <div id='app'></div>
        <script>var app = Elm.Main.embed(document.getElementById('app'));</script>
        <button onclick='app.ports.input.send(1);'>send</button>
    </body>
    </html>

  [1]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build
  [2]: https://www.wikiod.com/elm/subscriptions
  [3]: https://www.wikiod.com/elm/ports-js-interop
  [4]: https://www.wikiod.com/elm/getting-started-with-elm-language#Embedding into HTML

## One way parent-child communication
Example demonstrates component composition and one-way message passing from parent to children.

<!-- if version [lt 0.18.0] -->
Component composition relies on Message tagging with `Html.App.map`
<!-- end version if -->

<!-- if version [gte 0.18.0] -->
_In `0.18.0` `HTML.App` [was collapsed into][1] `HTML`_

Component composition relies on Message tagging with `Html.map`
<!-- end version if -->

# Example
See how to build the example in [Initialise and build][2]

    module Main exposing (..)
    
    import Html exposing (text, div, button, Html)
    import Html.Events exposing (onClick)
    import Html.App exposing (beginnerProgram)
    
    
    main =
        beginnerProgram
            { view = view
            , model = init
            , update = update
            }
    
    {- In v0.18.0 HTML.App was collapsed into HTML
       Use Html.map instead of Html.App.map
    -}
    view : Model -> Html Msg
    view model =
        div []
            [ Html.App.map FirstCounterMsg (counterView model.firstCounter)
            , Html.App.map SecondCounterMsg (counterView model.secondCounter)
            , button [ onClick ResetAll ] [ text "Reset counters" ]
            ]
    
    
    type alias Model =
        { firstCounter : CounterModel
        , secondCounter : CounterModel
        }
    
    
    init : Model
    init =
        { firstCounter = 0
        , secondCounter = 0
        }
    
    
    type Msg
        = FirstCounterMsg CounterMsg
        | SecondCounterMsg CounterMsg
        | ResetAll
    
    
    update : Msg -> Model -> Model
    update msg model =
        case msg of
            FirstCounterMsg childMsg ->
                { model | firstCounter = counterUpdate childMsg model.firstCounter }
    
            SecondCounterMsg childMsg ->
                { model | secondCounter = counterUpdate childMsg model.secondCounter }
    
            ResetAll ->
                { model
                    | firstCounter = counterUpdate Reset model.firstCounter
                    , secondCounter = counterUpdate Reset model.secondCounter
                }
    
    
    type alias CounterModel =
        Int
    
    
    counterView : CounterModel -> Html CounterMsg
    counterView model =
        div []
            [ button [ onClick Decrement ] [ text "-" ]
            , text (toString model)
            , button [ onClick Increment ] [ text "+" ]
            ]
    
    
    type CounterMsg
        = Increment
        | Decrement
        | Reset
    
    
    counterUpdate : CounterMsg -> CounterModel -> CounterModel
    counterUpdate msg model =
        case msg of
            Increment ->
                model + 1
    
            Decrement ->
                model - 1
    
            Reset ->
                0


  [1]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md#package-changes "Package Changes"
  [2]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build

## Message tagging with Html.App.map
Components define their own Messages, sent after emitted DOM Events, eg. `CounterMsg` from [Parent-child communication][1]

    type CounterMsg
        = Increment
        | Decrement
        | Reset

The view of this component will send messages of `CounterMsg` type, therefore the view type signature is `Html CounterMsg`.

To be able to reuse `counterView` inside parent component's view, we need to pass every `CounterMsg` message through parent's `Msg`.

This technique is called ***message tagging***.

Parent component must define messages for passing child messages:

    type Msg
        = FirstCounterMsg CounterMsg
        | SecondCounterMsg CounterMsg
        | ResetAll

`FirstCounterMsg Increment` is a tagged message.

<!-- if version [lt 0.18.0] -->
To get a `counterView` to send tagged messages, we must use the `Html.App.map` function:

    Html.map FirstCounterMsg (counterView model.firstCounter)

<!-- end version if -->
<!-- if version [gte 0.18.0] -->
_The `HTML.App` package [was collapsed][2] into the `HTML` package in `v0.18.0`_

To get a `counterView` to send tagged messages, we must use the `Html.map` function:

    Html.map FirstCounterMsg (counterView model.firstCounter)

<!-- end version if -->
That changes the type signature `Html CounterMsg -> Html Msg` so it's possible to use the counter inside the parent view and handle state updates with parent's update function.


  [1]: https://www.wikiod.com/elm/the-elm-architecture#One way parent-child communication
  [2]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md#package-changes "Package Changes"

