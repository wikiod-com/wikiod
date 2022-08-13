---
title: "Subscriptions"
slug: "subscriptions"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

Subscriptions are means to listen to inputs. [Incoming ports][1], keyboard or mouse events, WebSocket messages, geolocation and page visibility changes, all can serve as inputs.


  [1]: https://www.wikiod.com/elm/ports-js-interop#Incoming

## Basic subscription to Time.every event with 'unsubscribe'
<!-- if version [gte 0.18.0] -->
Model is passed to subscriptions which means that every state change can modify subscriptions.
<!-- language: lang-elm -->
    import Html exposing ( Html, div, text, button )
    import Html.Events exposing ( onClick )
    import Time

    main : Program Never Model Msg
    main =
        Html.program
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }

    -- MODEL

    type alias Model =
        { time: Time.Time
        , suspended: Bool
        }

    init : (Model, Cmd Msg)
    init =
        ( Model 0 False, Cmd.none )

    -- UPDATE

    type Msg
        = Tick Time.Time
        | SuspendToggle

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Tick newTime ->
                ( { model | time = newTime }, Cmd.none )

            SuspendToggle ->
                ( { model | suspended = not model.suspended }, Cmd.none )

    -- SUBSCRIPTIONS

    subscriptions : Model -> Sub Msg
    subscriptions model =
        if model.suspended then
            Sub.none
        else
            Time.every Time.second Tick

    -- VIEW

    view : Model -> Html Msg
    view model =
        div []
            [ div [] [ text <| toString model ]
            , button [ onClick SuspendToggle ] [ text ( if model.suspended then "Resume" else "Suspend" ) ]
            ]
<!-- end version if -->


