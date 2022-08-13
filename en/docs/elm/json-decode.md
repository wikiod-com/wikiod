---
title: "Json.Decode"
slug: "jsondecode"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

`Json.Decode` exposes two functions to decode a payload, first one is `decodeValue` which tries to decode a `Json.Encode.Value`, the second one is `decodeString` which tries to decode a JSON string. Both function take 2 parameters, a decoder and a `Json.Encode.Value` or Json string.



## Pre-decode a field and decode the rest depending on that decoded value
The following examples can be tested on https://ellie-app.com/m9vmQ8NcMc/0.

<!-- language: lang-elm -->
    import Html exposing (..)
    import Json.Decode
    
    payload =
      """
      [ { "bark": true, "tag": "dog", "name": "Zap", "playful": true }
      , { "whiskers": true, "tag" : "cat", "name": "Felix" }
      , {"color": "red", "tag": "tomato"}
      ]
      """
    
    -- OUR MODELS
    
    type alias Dog =
      { bark: Bool
      , name: String
      , playful: Bool
      }
      
    type alias Cat =
      { whiskers: Bool
      , name: String
      }
    
    -- OUR DIFFERENT ANIMALS
    
    type Animal 
      = DogAnimal Dog
      | CatAnimal Cat
      | NoAnimal
    
    main =
      Json.Decode.decodeString decoder payload
      |> toString
      |> text
    
    decoder =
      Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen animalType
        |> Json.Decode.list 
    
    animalType tag =
      case tag of
        "dog" ->
          Json.Decode.map3 Dog 
              (Json.Decode.field "bark" Json.Decode.bool) 
              (Json.Decode.field "name" Json.Decode.string) 
              (Json.Decode.field "playful" Json.Decode.bool) 
            |> Json.Decode.map DogAnimal
        "cat" ->
          Json.Decode.map2 Cat 
              (Json.Decode.field "whiskers" Json.Decode.bool) 
              (Json.Decode.field "name" Json.Decode.string)
            |> Json.Decode.map CatAnimal
        _ ->
          Json.Decode.succeed NoAnimal

## Decoding a list
The following example can be tested on https://ellie-app.com/m9tk39VpQg/0.

<!-- language: lang-elm -->
    import Html exposing (..)
    import Json.Decode 
    
    payload =
      """
      ["fu", "bar"]
      """
    
    main =
      Json.Decode.decodeString decoder payload -- Ok ["fu","bar"]
      |> toString
      |> text
    
    decoder =
      Json.Decode.list Json.Decode.string

## Decoding a list of records
The following code can be found in a demo here: https://ellie-app.com/mbFwJT9jD3/0
<!-- language: lang-elm -->
    import Html exposing (..)
    import Json.Decode exposing (Decoder)

    payload =
      """
      [{
          "id": 0,
          "name": "Adam Carter",
          "work": "Unilogic",
          "email": "adam.carter@unilogic.com",
          "dob": "24/11/1978",
          "address": "83 Warner Street",
          "city": "Boston",
          "optedin": true
        },
        {
          "id": 1,
          "name": "Leanne Brier",
          "work": "Connic",
          "email": "leanne.brier@connic.org",
          "dob": "13/05/1987",
          "address": "9 Coleman Avenue",
          "city": "Toronto",
          "optedin": false
        }]
      """
      
    type alias User =
      { name: String
      , work: String
      , email: String
      , dob: String
      , address: String
      , city: String
      , optedin: Bool
      }
     
    main =
      Json.Decode.decodeString decoder payload
      |> toString
      |> text
    
    decoder: Decoder (List User)
    decoder =
        Json.Decode.map7 User 
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "work" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "dob" Json.Decode.string)
        (Json.Decode.field "address" Json.Decode.string)
        (Json.Decode.field "city" Json.Decode.string)
        (Json.Decode.field "optedin" Json.Decode.bool)
        |> Json.Decode.list 
      




## Decoding JSON from Rust enum

This is useful if you use rust in the backend and elm on the front end

<!-- language: lang-js -->
    enum Complex{
        Message(String),
        Size(u64)
    }
    
    let c1 = Complex::Message("hi");
    let c2 = Complex::Size(1024u64);


The encoded Json from rust will be:

<!-- language: lang-js -->
    c1:
        {"variant": "Message",
         "fields": ["hi"]
        }
    c2:
        {"variant": "Size",
         "fields": [1024]
        }


The decoder in elm


<!-- language: lang-elm -->
 
    import Json.Decode as Decode exposing (Decoder)
    
    type Complex = Message String
        | Size Int

    -- decodes json to Complex type
    complexDecoder: Decoder Value
    complexDecoder = 
        ("variant" := Decode.string `Decode.andThen` variantDecoder)
    
    variantDecoder: String -> Decoder Value
    variantDecoder variant =
        case variant of
            "Message" ->
                Decode.map Message 
                    ("fields" := Decode.tuple1 (\a -> a) Decode.string)
            "Size" ->
                Decode.map Size
                    ("fields" := Decode.tuple1 (\a -> a) Decode.int)
            _ ->
                Debug.crash "This can't happen"

Usage: the data is requested from http rest api and the decoding of the payload will be

<!-- language: lang-hs -->

        Http.fromJson complexDecoder payload
    
Decoding from string will be

<!-- language: lang-hs -->
        Decode.decodeString complexDecoder payload

## Decode a Date
In case you have json with an ISO date string like this

<!-- language: lang-js -->

    JSON.stringify({date: new Date()})
    // -> "{"date":"2016-12-12T13:24:34.470Z"}"

You can map it to elm `Date` type:

<!-- language: lang-elm -->

    import Html exposing (text)
    import Json.Decode as JD
    import Date

    payload = """{"date":"2016-12-12T13:24:34.470Z"}"""

    dateDecoder : JD.Decoder Date.Date
    dateDecoder =
      JD.string
        |> JD.andThen ( \str ->
              case Date.fromString str of
                Err err -> JD.fail err
                Ok date -> JD.succeed date )

    payloadDecoder : JD.Decoder Date.Date
    payloadDecoder =
      JD.field "date" dateDecoder

    main =
      JD.decodeString payloadDecoder payload
      |> toString
      |> text
      

## Decode a List of Objects Containing Lists of Objects
*See [Ellie][1] for a working example.* This example uses the [NoRedInk/elm-decode-pipeline][pipeline] module.

Given a list of JSON objects, which themselves contain lists of JSON objects:


    [
      {
        "id": 0,
        "name": "Item 1",
        "transactions": [
          { "id": 0, "amount": 75.00 },
          { "id": 1, "amount": 25.00 }
        ]
      },
      {
        "id": 1,
        "name": "Item 2",
        "transactions": [
          { "id": 0, "amount": 50.00 },
          { "id": 1, "amount": 15.00 }
        ]
      }
    ]

If the above string is in the `payload` string, that can be decoded using the following:

    module Main exposing (main)
    
    import Html exposing (..)
    import Json.Decode as Decode exposing (Decoder)
    import Json.Decode.Pipeline as JP
    import String


    type alias Item =
        { id : Int
        , name : String
        , transactions : List Transaction
        }


    type alias Transaction =
        { id : Int
        , amount : Float
        }


    main =
        Decode.decodeString (Decode.list itemDecoder) payload
            |> toString
            |> String.append "JSON "
            |> text



    itemDecoder : Decoder Item
    itemDecoder =
        JP.decode Item
            |> JP.required "id" Decode.int
            |> JP.required "name" Decode.string
            |> JP.required "transactions" (Decode.list transactionDecoder)


    transactionDecoder : Decoder Transaction
    transactionDecoder =
        JP.decode Transaction
            |> JP.required "id" Decode.int
            |> JP.required "amount" Decode.float




[1]: https://ellie-app.com/qw3Hmcp3NGa1/0
[pipeline]: https://github.com/NoRedInk/elm-decode-pipeline

