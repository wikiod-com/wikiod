---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9842
type: docs
toc: true
---

## Syntax
 - Debug.log "tag" anyValue

`Debug.log` takes two parameters, a `String` to tag the debug output in the console (so you know where it's coming from / what the message corresponds to), and a value of any type. `Debug.log` executes the side-effect of logging the tag and the value to the JavaScript console, and then returns the value. The implementation in JS might look something like:

    function log (tag, value){
        console.log(tag, value);
        return value
    }


JavaScript has implicit conversions, so `value` doesn't have to be explicitly converted to a `String` for the above code to work. However, Elm types must be explicitly converted to a `String`, and the Native code for [`Debug.log`](https://github.com/elm-lang/core/blob/4.0.3/src/Native/Debug.js#L7) shows this in action.

## Logging a value without interrupting computations
`Debug.log`'s second argument is always returned, so you could write code like the following and it would *just work*:

<!-- language: lang-hs -->
    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case Debug.log "The Message" msg of
            Something ->
                ...

Replacing `case msg of` with `case Debug.log "The Message" msg of` will cause the current message to be logged the console every time the update function is called, but changes nothing else.

## Piping a Debug.log
At run time the following would display a list of url in your console and continue computation

<!-- language: lang-hs -->

    payload =
        [{url:..., title:...}, {url=..., title=...}]

    main = 
        payload
            |> List.map .url -- only takes the url
            |> Debug.log " My list of URLs" -- pass the url list to Debug.log and return it
            |> doSomething -- do something with the url list


## Time-traveling debugger
<!-- if version [gte 0.17] [lt 0.18.0] -->
At the time of writing (July 2016) [elm-reactor][1] has been temporarily stripped of its time traveling functionality. It's possible to get it, though, using the [`jinjor/elm-time-travel`][2] package.

It's usage mirrors [`Html.App`][3] or [`Navigation`][4] modules' `program*` functions, for example instead of:

<!-- language: lang-hs -->

    import Html.App

    main =
        Html.App.program
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            }

you'd write:

<!-- language: lang-hs -->

    import TimeTravel.Html.App

    main =
        TimeTravel.Html.App.program
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            }

(Of course, after installing the package with `elm-package`.)

The interface of your app changes as a result, [see one of the demos][5].

<!-- end version if -->
<!-- if version [gte 0.18.0] -->
Since version **0.18.0** you can simply can compile your program with the `--debug` flag and get [time travel debugging][6] with no additional effort.
<!-- end version if -->


  [1]: https://github.com/elm-lang/elm-reactor#note-about-time-travel
  [2]: http://package.elm-lang.org/packages/jinjor/elm-time-travel/latest/
  [3]: https://www.wikiod.com/elm/the-elm-architecture
  [4]: http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation
  [5]: http://jinjor.github.io/elm-time-travel/
  [6]: http://elm-lang.org/blog/the-perfect-bug-report "the perfect bug report"

## Debug.Crash
    case thing of
        Cat ->
            meow
        Bike ->
            ride
        Sandwich ->
            eat
        _ ->
            Debug.crash "Not yet implemented"

You can use [`Debug.crash`][1] when you want the program to fail, typically used when you're in the middle of implementing a `case` expression.  It is _not_ recommended to use `Debug.crash` instead of using a `Maybe` or `Result` type for unexpected inputs, but typically only during the course of development (i.e. you typically wouldn't publish Elm code which uses `Debug.crash`).

`Debug.crash` takes one `String` value, the error message to show when crashing.  Note that Elm will also output the name of the module and the line of the crash, and if the crash is in a `case` expression, it will indicate the value of the `case`.

  [1]: http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash

