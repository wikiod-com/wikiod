---
title: "Custom JSON Decoders"
slug: "custom-json-decoders"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

How to use Json.Decode to create custom decoders, for example decoding into union types and user defined data types

## Decoding into union type
    import Json.Decode as JD
    import Json.Decode.Pipeline as JP

    type PostType = Image | Video
     
    type alias Post = { 
        id: Int
        , postType: PostType
    }
    -- assuming server will send int value of 0 for Image or 1 for Video
    decodePostType: JD.Decoder PostType
    decodePostType = 
        JD.int |> JD.andThen (\postTypeInt -> 
            case postTypeInt of 
                0 ->
                    JD.succeed Image


                1 ->
                    JD.succed Video

                _ ->
                    JD.fail "invalid posttype"

        )

    decodePostMap : JD.Decoder Post
    decodePostMap = 
        JD.map2 Post
            (JD.field "id" JD.int)
            (JD.field "postType" decodePostType)

    decodePostPipline : JD.Decoder Post
    decodePostPipline = 
        JP.decode Post
            |> JP.required "id" JD.int
            |> JP.required "postType" decodePostType
            


