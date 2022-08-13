---
title: "Making complex update functions with ccapndaveelm-update-extra"
slug: "making-complex-update-functions-with-ccapndaveelm-update-extra"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

ccapndave/elm-update-extra is a fantastic package which helps you handle more complex updating functions, and may be very useful.

## Message which call a list of messages
Using `sequence` function you can easily describe a message that calls a list of other messages. It's useful when dealing with semantics of your messages.

Example 1: You are making a game engine, and you need to refresh the screen on every frame.

<!-- language: lang-elm -->
    module Video exposing (..)
    type Message = module Video exposing (..)
    
    import Update.Extra exposing (sequence)

    -- Model definition [...]
    
    type Message
        = ClearBuffer
        | DrawToBuffer
        | UpdateLogic
        | Update
    
    update : Message -> Model -> (Model, Cmd)
    update msg model =
        case msg of
            ClearBuffer ->
                -- do something
            DrawToBuffer ->
                -- do something
            UpdateLogic ->
                -- do something
            Update ->
                model ! []
                    |> sequence update [ ClearBuffer
                                       , DrawToBuffer
                                       , UpdateLogic]

## Chaining messages with andThen
The `andThen` function allows update call composition. Can be used with the pipeline operator (`|>`) to chain updates.

Example: You are making a document editor, and you want that each modification message you send to your document, you also save it:

    import Update.Extra exposing (andThen)
    import Update.Extra.Infix exposing (..)
    
    -- type alias Model = [...]
    
    type Message
        = ModifyDocumentWithSomeSettings
        | ModifyDocumentWithOtherSettings
        | SaveDocument
    
    update : Model -> Message -> (Model, Cmd)
    update model msg =
        case msg of
            ModifyDocumentWithSomeSettings ->
                -- make the modifications
                (modifiedModel, Cmd.none)
                |> andThen SaveDocument
            ModifyDocumentWithOtherSettings ->
                -- make other modifications
                (modifiedModel, Cmd.none)
                |> andThen SaveDocument
            SaveDocument ->
                -- save document code

If you import also `Update.Extra.Infix exposing (..)` you may be able to use the infix operator:

    update : Model -> Message -> (Model, Cmd)
    update model msg =
        case msg of
            ModifyDocumentWithSomeSettings ->
                -- make the modifications
                (modifiedModel, Cmd.none)
                :> andThen SaveDocument
            ModifyDocumentWithOtherSettings ->
                -- make other modifications
                (modifiedModel, Cmd.none)
                :> SaveDocument
            SaveDocument ->
                -- save document code

