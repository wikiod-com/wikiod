---
title: "Introduction to WPF in F#"
slug: "introduction-to-wpf-in-f"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

This topic illustrates how to exploit **Functional Programming** in a **WPF application**. The first example comes from a post by Māris Krivtežs (ref *Remarks* section at the bottom). The reason for revisiting this project is twofold:

1\ The design supports separation of concerns, while the model is kept pure and changes are propagated in a functional way.

2\ The resemblance will make for an easy transition to the Gjallarhorn implementation.


Library demo projects @GitHub
- [FSharp.ViewModule](https://github.com/fsprojects/FsXaml) (under FsXaml)
- [Gjallarhorn](https://github.com/ReedCopsey/Gjallarhorn) (ref Samples)

Māris Krivtežs wrote two great posts on this topic:
- [F# XAML application - MVVM vs MVC](http://marisks.net/2015/04/27/f-sharp-xaml-application-mvvm-vs-mvc/) where the pros and cons of both approaches are highlighted. 

> I feel that none of these XAML application
> styles benefit much from functional programming. I imagine that the
> ideal application would consist of the view which produces events and
> events hold current view state. All application logic should be
> handled by filtering and manipulating events and view model, and in
> the output it should produce a new view model which is bound back to
> the view.

- [F# XAML - event driven MVVM]( http://marisks.net/2015/05/11/f-sharp-xaml-event-driven-mvvm/) as revisited in the topic above. 


## FSharp.ViewModule
Our demo app consists of a scoreboard. The score model is an immutable record. The scoreboard events are contained in a Union Type.

    namespace Score.Model
    
    type Score = { ScoreA: int ; ScoreB: int }    
    type ScoringEvent = IncA | DecA | IncB | DecB | NewGame

Changes are propagated by listening for events and updating the view model accordingly. Instead of adding members to the model type, as in OOP, we declare a separate module to host the allowed operations.

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Score =
        let zero = {ScoreA = 0; ScoreB = 0}
        let update score event =
            match event with
            | IncA -> {score with ScoreA = score.ScoreA + 1}
            | DecA -> {score with ScoreA = max (score.ScoreA - 1) 0}
            | IncB -> {score with ScoreB = score.ScoreB + 1}
            | DecB -> {score with ScoreB = max (score.ScoreB - 1) 0}
            | NewGame -> zero 

Our view model derives from `EventViewModelBase<'a>`, which has a property `EventStream` of type `IObservable<'a>`. In this case the events we want to subscribe to are of type `ScoringEvent`. 

The controller handles the events in a functional manner. Its signature `Score -> ScoringEvent -> Score` shows us that, whenever an event occurs, the current value of the model is transformed into a new value. This allows for our model to remain pure, although our view model is not.

An `eventHandler` is in charge of mutating the state of the view. Inheriting from `EventViewModelBase<'a>` we can use `EventValueCommand` and  `EventValueCommandChecked` to hook up the events to the commands.

    namespace Score.ViewModel
    
    open Score.Model
    open FSharp.ViewModule
    
    type MainViewModel(controller : Score -> ScoringEvent -> Score) as self = 
        inherit EventViewModelBase<ScoringEvent>()
    
        let score = self.Factory.Backing(<@ self.Score @>, Score.zero)
    
        let eventHandler ev = score.Value <- controller score.Value ev
    
        do
            self.EventStream
            |> Observable.add eventHandler
    
        member this.IncA = this.Factory.EventValueCommand(IncA)
        member this.DecA = this.Factory.EventValueCommandChecked(DecA, (fun _ -> this.Score.ScoreA > 0), [ <@@ this.Score @@> ])
        member this.IncB = this.Factory.EventValueCommand(IncB)
        member this.DecB = this.Factory.EventValueCommandChecked(DecB, (fun _ -> this.Score.ScoreB > 0), [ <@@ this.Score @@> ])
        member this.NewGame = this.Factory.EventValueCommand(NewGame)
    
        member __.Score = score.Value

The code behind file (*.xaml.fs) is where everything is put together, i.e. the update function (`controller`) is injected in the `MainViewModel`.

    namespace Score.Views
    
    open FsXaml
    
    type MainView = XAML<"MainWindow.xaml">
    
    type CompositionRoot() =
        member __.ViewModel = Score.ViewModel.MainViewModel(Score.Model.Score.update)

The type `CompositionRoot` serves as a wrapper that’s referenced in the XAML file.

    <Window.Resources>
        <ResourceDictionary>
            <local:CompositionRoot x:Key="CompositionRoot"/>
        </ResourceDictionary>
    </Window.Resources>
    <Window.DataContext>
        <Binding Source="{StaticResource CompositionRoot}" Path="ViewModel" />
    </Window.DataContext>

I won’t dive any deeper into the XAML file as it’s basic WPF stuff, the entire project can be found on [GitHub](https://github.com/marisks/evented_mvvm/tree/mvc_refactored).


## Gjallarhorn
The core types in the [Gjallarhorn library](http://reedcopsey.github.io/Gjallarhorn/index.html) implement `IObservable<'a>`, which will make the implementation look familiar (remember the `EventStream` property from the FSharp.ViewModule example). The only real change to our model is the order of the arguments of the update function. Also, we now use the term *Message* instead of *Event*.

    namespace ScoreLogic.Model
    
    type Score = { ScoreA: int ; ScoreB: int }    
    type ScoreMessage = IncA | DecA | IncB | DecB | NewGame
    
    // Module showing allowed operations
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Score =
        let zero = {ScoreA = 0; ScoreB = 0}
        let update msg score =
            match msg with
            | IncA -> {score with ScoreA = score.ScoreA + 1}
            | DecA -> {score with ScoreA = max (score.ScoreA - 1) 0}
            | IncB -> {score with ScoreB = score.ScoreB + 1}
            | DecB -> {score with ScoreB = max (score.ScoreB - 1) 0}
            | NewGame -> zero

In order to build a UI with Gjallarhorn, instead of making classes to support data binding, we create simple functions referred to as a `Component`. In their constructor the first argument `source` is of type `BindingSource` (defined in Gjallarhorn.Bindable), and used to map the model to the view, and events from the view back into messages.

    namespace ScoreLogic.Model
    
    open Gjallarhorn
    open Gjallarhorn.Bindable
    
    module Program =
    
        // Create binding for entire application.
        let scoreComponent source (model : ISignal<Score>) =
            // Bind the score to the view
            model |> Binding.toView source "Score"
    
            [
                // Create commands that turn into ScoreMessages
                source |> Binding.createMessage "NewGame" NewGame 
                source |> Binding.createMessage "IncA" IncA
                source |> Binding.createMessage "DecA" DecA
                source |> Binding.createMessage "IncB" IncB
                source |> Binding.createMessage "DecB" DecB
            ]

The current implementation differs from the FSharp.ViewModule version in that two commands do not have CanExecute properly implemented yet. Also listing the application’s plumbing.

    namespace ScoreLogic.Model
    
    open Gjallarhorn
    open Gjallarhorn.Bindable
    
    module Program =
    
        // Create binding for entire application.
        let scoreComponent source (model : ISignal<Score>) =
            let aScored = Mutable.create false
            let bScored = Mutable.create false
    
            // Bind the score itself to the view
            model |> Binding.toView source "Score"
    
            // Subscribe to changes of the score
            model |> Signal.Subscription.create 
                (fun currentValue -> 
                    aScored.Value <- currentValue.ScoreA > 0
                    bScored.Value <- currentValue.ScoreB > 0) 
                |> ignore
    
            [
                // Create commands that turn into ScoreMessages
                source |> Binding.createMessage "NewGame" NewGame 
                source |> Binding.createMessage "IncA" IncA
                source |> Binding.createMessageChecked "DecA" aScored DecA
                source |> Binding.createMessage "IncB" IncB
                source |> Binding.createMessageChecked "DecB" bScored DecB
            ]
    
        let application = 
            // Create our score, wrapped in a mutable with an atomic update function
            let score = new AsyncMutable<_>(Score.zero)
    
            // Create our 3 functions for the application framework
    
            // Start with the function to create our model (as an ISignal<'a>)
            let createModel () : ISignal<_> = score :> _
    
            // Create a function that updates our state given a message
            // Note that we're just taking the message, passing it directly to our model's update function,
            // then using that to update our core "Mutable" type.
            let update (msg : ScoreMessage) : unit = Score.update msg |> score.Update |> ignore
    
            // An init function that occurs once everything's created, but before it starts
            let init () : unit = ()
    
            // Start our application
            Framework.application createModel init update scoreComponent

Left with setting up the decoupled view, combining the `MainWindow` type and the logical application.

    namespace ScoreBoard.Views
    
    open System
    
    open FsXaml
    open ScoreLogic.Model
    
    // Create our Window
    type MainWindow = XAML<"MainWindow.xaml"> 
    
    module Main =    
        [<STAThread>]
        [<EntryPoint>]
        let main _ =  
            // Run using the WPF wrappers around the basic application framework    
            Gjallarhorn.Wpf.Framework.runApplication System.Windows.Application MainWindow Program.application

This sums up the core concepts, for additional information and a more elaborate example please refer to [Reed Copsey’s post]( http://reedcopsey.com/2016/12/15/christmas-trees-in-wpf-2016-edition/#more-365). The *Christmas Trees* project highlights a couple of benefits to this approach:

 - Effectively redeeming us from the need to (manually) copy the model
   into a custom collection of view models, managing them, and manually
   constructing the model back from the results.
 - Updates within collections are done in a transparent manner, while maintaining a pure model.
 - The logic and view are hosted by two different projects, emphasizing the separation of concerns.


