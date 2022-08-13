---
title: "Pattern Matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Single type deconstructed argument
    type ProjectIdType = ProjectId String

    getProject : ProjectIdType -> Cmd Msg
    getProject (ProjectId id) =
        Http.get <| "/projects/" ++ id


## Function arguments
<!-- language: lang-elm -->

    type Dog = Dog String

    dogName1 dog =
        case dog of
          Dog name ->
             name

    dogName2 (Dog name) ->
         name

`dogName1` and `dogName2` are equivalent. Note that this only works for ADTs that have a single constructor.

<!-- language: lang-elm -->

    type alias Pet =
       { name: String
       , weight: Float
       
       }

    render : Pet -> String
    render ({name, weight} as pet) =
        (findPetEmoji pet) ++ " " ++ name ++ " weighs " ++ (toString weight)

    findPetEmoji : Pet -> String
    findPetEmoji pet = 
        Debug.crash "Implementation TBD"

Here we deconstruct a record and also get a reference to the undeconstructed record.

