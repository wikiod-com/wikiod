---
title: "Backend Integration"
slug: "backend-integration"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Basic elm Http.post json request to node.js express server
<!-- if version [gte 0.18.0] -->
Live upcase server that returns error when input string is longer than 10 characters.

**Server:**
<!-- language: lang-js -->
    const express = require('express'),
        jsonParser = require('body-parser').json(),
        app = express();

    // Add headers to work with elm-reactor
    app.use((req, res, next) => {
        res.setHeader('Access-Control-Allow-Origin', 'http://localhost:8000');
        res.setHeader('Access-Control-Allow-Methods', 'POST, OPTIONS');
        res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type');
        res.setHeader('Access-Control-Allow-Credentials', true);
        next();
    });

    app.post('/upcase', jsonParser, (req, res, next) => {
        // Just an example of possible invalid data for an error message demo
        if (req.body.input && req.body.input.length < 10) {
            res.json({
                output: req.body.input.toUpperCase()
            });
        } else {
            res.status(500).json({
                error: `Bad input: '${req.body.input}'`
            });
        }
    });

    const server = app.listen(4000, () => {
        console.log('Server is listening at http://localhost:4000/upcase');
    });

**Client:**
<!-- language: lang-elm -->
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (..)
    import Http
    import Json.Decode as JD
    import Json.Encode as JE

    main : Program Never Model Msg
    main =
        Html.program
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

    -- MODEL

    type alias Model =
        { output: String
        , error: Maybe String
        }

    init : (Model, Cmd Msg)
    init =
        ( Model "" Nothing
        , Cmd.none
        )

    -- UPDATE

    type Msg
        = UpcaseRequest ( Result Http.Error String )
        | InputString String

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            UpcaseRequest (Ok response) ->
                ( { model | output = response, error = Nothing }, Cmd.none )

            UpcaseRequest (Err err) ->
                let
                    errMsg = case err of
                        Http.Timeout ->
                            "Request timeout"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadPayload msg _ ->
                            msg

                        Http.BadStatus response ->
                            case JD.decodeString upcaseErrorDecoder response.body of
                                Ok errStr ->
                                    errStr

                                Err _ ->
                                    response.status.message

                        Http.BadUrl msg ->
                            "Bad url: " ++ msg
                in
                    ( { model | output = "", error = Just errMsg }, Cmd.none )

            InputString str ->
                ( model, upcaseRequest str )

    -- VIEW

    view : Model -> Html Msg
    view model =
        let
            outDiv = case model.error of
                Nothing ->
                    div []
                        [ label [ for "outputUpcase" ] [ text "Output" ]
                        , input [ type_ "text", id "outputUpcase", readonly True, value model.output ] []
                        ]

                Just err ->
                    div []
                        [ label [ for "errorUpcase" ] [ text "Error" ]
                        , input [ type_ "text", id "errorUpcase", readonly True, value err ] []
                        ]
        in
            div []
                [ div []
                    [ label [ for "inputToUpcase" ] [ text "Input" ]
                    , input [ type_ "text", id "inputToUpcase", onInput InputString ] []
                    ]
                , outDiv
                ]

    -- SUBSCRIPTIONS

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.none

    -- HELPERS

    upcaseSuccessDecoder : JD.Decoder String
    upcaseSuccessDecoder = JD.field "output" JD.string

    upcaseErrorDecoder : JD.Decoder String
    upcaseErrorDecoder = JD.field "error" JD.string

    upcaseRequestEncoder : String -> JE.Value
    upcaseRequestEncoder str = JE.object [ ( "input", JE.string str ) ]

    upcaseRequest : String -> Cmd Msg
    upcaseRequest str =
        let
            req = Http.post "http://localhost:4000/upcase" ( Http.jsonBody <| upcaseRequestEncoder str ) upcaseSuccessDecoder
        in
            Http.send UpcaseRequest req



