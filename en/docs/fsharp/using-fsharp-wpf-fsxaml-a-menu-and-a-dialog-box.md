---
title: "Using F#, WPF, FsXaml, a Menu, and a Dialog Box"
slug: "using-f-wpf-fsxaml-a-menu-and-a-dialog-box"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

The goal here is to build a simple application in F# using the Windows 
Presentation Foundation (WPF) with traditional menus and dialog boxes. It stems 
from my frustration in trying to wade through hundreds of sections of 
documentation, articles and posts dealing with F# and WPF. In order to do 
anything with WPF, you seem to have to know everything about it. My purpose here
is to provide a possible way in, a simple desktop project that can serve as a 
template for your apps.

## Set up the Project
We'll assume you're doing this in Visual Studio 2015 (VS 2015 Community, in my 
case). Create an empty Console project in VS. In Project | Properties change the
Output Type to Windows Application.

Next, use NuGet to add FsXaml.Wpf to the project; this package was created by
the estimable Reed Copsey, Jr., and it greatly simplifies using WPF from F#. On
installation, it will add a number of other WPF assemblies, so you will not have
to. There are other similar packages to FsXaml, but one of my goals was to keep 
the number of tools as small as possible in order to make the overall project as 
simple and maintaiable as possible.

In addition, add UIAutomationTypes as a reference; it comes as part of .NET.

## Add the "Business Logic"
Presumably, your program will do something. Add your working code to the project
in place of Program.fs. In this case, our task is to draw spirograph curves on a
Window Canvas. This is accomplished using Spirograph.fs, below.


    namespace Spirograph
    
    // open System.Windows does not automatically open all its sub-modules, so we 
    // have to open them explicitly as below, to get the resources noted for each.
    open System                             // for Math.PI
    open System.Windows                     // for Point
    open System.Windows.Controls            // for Canvas
    open System.Windows.Shapes              // for Ellipse
    open System.Windows.Media               // for Brushes
    
    // ------------------------------------------------------------------------------
    // This file is first in the build sequence, so types should be defined here
    type DialogBoxXaml  = FsXaml.XAML<"DialogBox.xaml">
    type MainWindowXaml = FsXaml.XAML<"MainWindow.xaml">
    type App            = FsXaml.XAML<"App.xaml"> 
    
    // ------------------------------------------------------------------------------
    // Model: This draws the Spirograph
    type MColor = | MBlue   | MRed | MRandom
    
    type Model() =
      let mutable myCanvas: Canvas = null
      let mutable myR              = 220    // outer circle radius
      let mutable myr              = 65     // inner circle radius
      let mutable myl              = 0.8    // pen position relative to inner circle
      let mutable myColor          = MBlue  // pen color
      
      let rng                      = new Random()
      let mutable myRandomColor    = Color.FromRgb(rng.Next(0, 255) |> byte,
                                                   rng.Next(0, 255) |> byte,
                                                   rng.Next(0, 255) |> byte)
    
      member this.MyCanvas
        with get() = myCanvas
        and  set(newCanvas) = myCanvas <- newCanvas
    
      member this.MyR
        with get() = myR
        and  set(newR) = myR <- newR
    
      member this.Myr
        with get() = myr
        and  set(newr) = myr <- newr
    
      member this.Myl
        with get() = myl
        and  set(newl) = myl <- newl
    
      member this.MyColor
        with get() = myColor
        and  set(newColor) = myColor <- newColor
    
      member this.Randomize =
        // Here we randomize the parameters. You can play with the possible ranges of
        // the parameters to find randomized spirographs that are pleasing to you.
        this.MyR      <- rng.Next(100, 500)
        this.Myr      <- rng.Next(this.MyR / 10, (9 * this.MyR) / 10)
        this.Myl      <- 0.1 + 0.8 * rng.NextDouble()
        this.MyColor  <- MRandom
        myRandomColor <- Color.FromRgb(rng.Next(0, 255) |> byte,
                                       rng.Next(0, 255) |> byte,
                                       rng.Next(0, 255) |> byte)
    
      member this.DrawSpirograph =
        // Draw a spirograph. Note there is some fussing with ints and floats; this 
        // is required because the outer and inner circle radii are integers. This is
        // necessary in order for the spirograph to return to its starting point 
        // after a certain number of revolutions of the outer circle.
        
        // Start with usual recursive gcd function and determine the gcd of the inner
        // and outer circle radii. Everything here should be in integers.
        let rec gcd x y =
            if y = 0 then x
            else gcd y (x % y)
      
        let g = gcd this.MyR this.Myr             // find greatest common divisor
        let maxRev = this.Myr / g                 // maximum revs to repeat
    
        // Determine width and height of window, location of center point, scaling 
        // factor so that spirograph fits within the window, ratio of inner and outer
        // radii.
        
        // Everything from this point down should be float.
        let width, height = myCanvas.ActualWidth, myCanvas.ActualHeight
        let cx, cy = width / 2.0, height / 2.0    // coordinates of center point
        let maxR   = min cx cy                    // maximum radius of outer circle
        let scale  = maxR / float(this.MyR)             // scaling factor
        let rRatio = float(this.Myr) / float(this.MyR)  // ratio of the radii
    
        // Build the collection of spirograph points, scaled to the window.
        let points = new PointCollection()
        for degrees in [0 .. 5 .. 360 * maxRev] do
          let angle = float(degrees) * Math.PI / 180.0
          let x, y = cx + scale * float(this.MyR) *
                     ((1.0-rRatio)*Math.Cos(angle) +
                      this.Myl*rRatio*Math.Cos((1.0-rRatio)*angle/rRatio)),
                     cy + scale * float(this.MyR) *
                     ((1.0-rRatio)*Math.Sin(angle) - 
                      this.Myl*rRatio*Math.Sin((1.0-rRatio)*angle/rRatio))
          points.Add(new Point(x, y))
      
        // Create the Polyline with the above PointCollection, erase the Canvas, and 
        // add the Polyline to the Canvas Children
        let brush = match this.MyColor with
                    | MBlue   -> Brushes.Blue
                    | MRed    -> Brushes.Red
                    | MRandom -> new SolidColorBrush(myRandomColor)
      
        let mySpirograph = new Polyline()
        mySpirograph.Points <- points
        mySpirograph.Stroke <- brush
    
        myCanvas.Children.Clear()
        this.MyCanvas.Children.Add(mySpirograph) |> ignore

Spirograph.fs is the first F# file in the compilation order, so it contains the 
definitions of the types we will need. Its job is to draw a spirograph on the 
main window Canvas based on parameters entered in a dialog box. Since there are
lots of references on how to draw a spirograph, we won't go into that here.



## Create the main window in XAML
You have to create a XAML file that defines the main window that contains our 
menu and drawing space. Here's the XAML code in MainWindow.xaml:

    <!-- This defines the main window, with a menu and a canvas. Note that the Height
         and Width are overridden in code to be 2/3 the dimensions of the screen -->
    <Window
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="Spirograph" Height="200" Width="300">
        <!-- Define a grid with 3 rows: Title bar, menu bar, and canvas. By default
             there is only one column -->
        <Grid>
            <Grid.RowDefinitions>
                <RowDefinition Height="Auto"/>
                <RowDefinition Height="*"/>
                <RowDefinition Height="Auto"/>
            </Grid.RowDefinitions>
            <!-- Define the menu entries -->
            <Menu  Grid.Row="0">
                <MenuItem Header="File">
                    <MenuItem Header="Exit"
                              Name="menuExit"/>
                </MenuItem>
                <MenuItem Header="Spirograph">
                    <MenuItem Header="Parameters..."
                              Name="menuParameters"/>
                    <MenuItem Header="Draw"
                              Name="menuDraw"/>
                </MenuItem>
                <MenuItem Header="Help">
                    <MenuItem Header="About"
                              Name="menuAbout"/>
                </MenuItem>
            </Menu>
            <!-- This is a canvas for drawing on. If you don't specify the coordinates
                 for Left and Top you will get NaN for those values -->
            <Canvas Grid.Row="1" Name="myCanvas" Left="0" Top="0">
            </Canvas>
        </Grid>
    </Window>

Comments are usually not included in XAML files, which I think is a mistake. I've
added some comments to all the XAML files in this project. I don't assert they are
the best comments ever written, but they at least show how a comment should be
formatted. Note that nested comments are not allowed in XAML.


## Create the dialog box in XAML and F#
The XAML file for the spirograph parameters is below. It includes three text
boxes for the spirograph parameters and a group of three radio buttons for color.
When we give radio buttons the same group name - as we have here - WPF handles
the on/off switching when one is selected.

    <!-- This first part is boilerplate, except for the title, height and width.
         Note that some fussing with alignment and margins may be required to get
         the box looking the way you want it. -->
    <Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            Title="Parameters" Height="200" Width="250">
        <!-- Here we define a layout of 3 rows and 2 columns below the title bar -->
        <Grid>
            <Grid.RowDefinitions>
                <RowDefinition/>
                <RowDefinition/>
                <RowDefinition/>
                <RowDefinition/>
                <RowDefinition/>
            </Grid.RowDefinitions>
            <Grid.ColumnDefinitions>
                <ColumnDefinition/>
                <ColumnDefinition/>
            </Grid.ColumnDefinitions>
            <!-- Define a label and a text box for the first three rows. Top row is
                 the integer radius of the outer circle -->
            <StackPanel Orientation="Horizontal" Grid.Column="0" Grid.Row="0" 
                        Grid.ColumnSpan="2">
                <Label VerticalAlignment="Top" Margin="5,6,0,1" Content="R: Outer" 
                       Height="24" Width='65'/>
                <TextBox x:Name="radiusR"  Margin="0,0,0,0.5" Width="120" 
                         VerticalAlignment="Bottom" Height="20">Integer</TextBox>
            </StackPanel>
            <!-- This defines a label and text box for the integer radius of the
                 inner circle -->
            <StackPanel Orientation="Horizontal" Grid.Column="0" Grid.Row="1" 
                        Grid.ColumnSpan="2">
                <Label VerticalAlignment="Top" Margin="5,6,0,1" Content="r: Inner" 
                       Height="24" Width='65'/>
                <TextBox x:Name="radiusr"  Margin="0,0,0,0.5" Width="120" 
                         VerticalAlignment="Bottom" Height="20" Text="Integer"/>
            </StackPanel>
            <!-- This defines a label and text box for the float ratio of the inner
                 circle radius at which the pen is positioned -->
            <StackPanel Orientation="Horizontal" Grid.Column="0" Grid.Row="2" 
                        Grid.ColumnSpan="2">
                <Label VerticalAlignment="Top" Margin="5,6,0,1" Content="l: Ratio" 
                       Height="24" Width='65'/>
                <TextBox x:Name="ratiol"  Margin="0,0,0,1" Width="120" 
                         VerticalAlignment="Bottom" Height="20" Text="Float"/>
            </StackPanel>
            <!-- This defines a radio button group to select color -->
            <StackPanel Orientation="Horizontal" Grid.Column="0" Grid.Row="3" 
                        Grid.ColumnSpan="2">
                <Label VerticalAlignment="Top" Margin="5,6,4,5.333" Content="Color" 
                       Height="24"/>
                <RadioButton x:Name="buttonBlue" Content="Blue" GroupName="Color" 
                             HorizontalAlignment="Left"  VerticalAlignment="Top"
                             Click="buttonBlueClick"
                             Margin="5,13,11,3.5" Height="17"/>
                <RadioButton x:Name="buttonRed"  Content="Red"  GroupName="Color" 
                             HorizontalAlignment="Left" VerticalAlignment="Top"
                             Click="buttonRedClick"
                             Margin="5,13,5,3.5" Height="17" />
                <RadioButton x:Name="buttonRandom"  Content="Random"  
                             GroupName="Color" Click="buttonRandomClick"
                             HorizontalAlignment="Left" VerticalAlignment="Top"
                             Margin="5,13,5,3.5" Height="17" />
            </StackPanel>
            <!-- These are the standard OK/Cancel buttons -->
            <Button Grid.Row="4" Grid.Column="0" Name="okButton" 
                    Click="okButton_Click" IsDefault="True">OK</Button>
            <Button Grid.Row="4" Grid.Column="1" Name="cancelButton" 
                    IsCancel="True">Cancel</Button>
        </Grid>
    </Window>

Now we add the code behind for the Dialog.Box. By convention, the code used to 
handle the interface of the dialog box with the rest of the program is named
XXX.xaml.fs, where the associated XAML file is named XXX.xaml.

    namespace Spirograph
    
    open System.Windows.Controls
    
    type DialogBox(app: App, model: Model, win: MainWindowXaml) as this =
      inherit DialogBoxXaml()
    
      let myApp   = app
      let myModel = model
      let myWin   = win
      
      // These are the default parameters for the spirograph, changed by this dialog
      // box
      let mutable myR = 220                 // outer circle radius
      let mutable myr = 65                  // inner circle radius
      let mutable myl = 0.8                 // pen position relative to inner circle
      let mutable myColor = MBlue           // pen color
    
      // These are the dialog box controls. They are initialized when the dialog box
      // is loaded in the whenLoaded function below.
      let mutable RBox: TextBox = null
      let mutable rBox: TextBox = null
      let mutable lBox: TextBox = null
    
      let mutable blueButton: RadioButton   = null
      let mutable redButton: RadioButton    = null
      let mutable randomButton: RadioButton = null
    
      // Call this functions to enable or disable parameter input depending on the
      // state of the randomButton. This is a () -> () function to keep it from
      // being executed before we have loaded the dialog box below and found the
      // values of TextBoxes and RadioButtons.
      let enableParameterFields(b: bool) = 
        RBox.IsEnabled <- b
        rBox.IsEnabled <- b
        lBox.IsEnabled <- b
    
      let whenLoaded _ =
        // Load and initialize text boxes and radio buttons to the current values in 
        // the model. These are changed only if the OK button is clicked, which is 
        // handled below. Also, if the color is Random, we disable the parameter
        // fields.
        RBox <- this.FindName("radiusR") :?> TextBox
        rBox <- this.FindName("radiusr") :?> TextBox
        lBox <- this.FindName("ratiol")  :?> TextBox
    
        blueButton   <- this.FindName("buttonBlue")   :?> RadioButton
        redButton    <- this.FindName("buttonRed")    :?> RadioButton
        randomButton <- this.FindName("buttonRandom") :?> RadioButton
    
        RBox.Text <- myModel.MyR.ToString()
        rBox.Text <- myModel.Myr.ToString()
        lBox.Text <- myModel.Myl.ToString()
    
        myR <- myModel.MyR
        myr <- myModel.Myr
        myl <- myModel.Myl
      
        blueButton.IsChecked   <- new System.Nullable<bool>(myModel.MyColor = MBlue)
        redButton.IsChecked    <- new System.Nullable<bool>(myModel.MyColor = MRed)
        randomButton.IsChecked <- new System.Nullable<bool>(myModel.MyColor = MRandom)
       
        myColor <- myModel.MyColor
        enableParameterFields(not (myColor = MRandom))
    
      let whenClosing _ =
        // Show the actual spirograph parameters in a message box at close. Note the 
        // \n in the sprintf gives us a linebreak in the MessageBox. This is mainly
        // for debugging, and it can be deleted.
        let s = sprintf "R = %A\nr = %A\nl = %A\nColor = %A" 
                        myModel.MyR myModel.Myr myModel.Myl myModel.MyColor
        System.Windows.MessageBox.Show(s, "Spirograph") |> ignore
        ()
      
      let whenClosed _ =
        () 
      
      do 
        this.Loaded.Add whenLoaded
        this.Closing.Add whenClosing
        this.Closed.Add whenClosed
    
      override this.buttonBlueClick(sender: obj, 
                                    eArgs: System.Windows.RoutedEventArgs) =
        myColor <- MBlue
        enableParameterFields(true)
        () 
      
      override this.buttonRedClick(sender: obj, 
                                   eArgs: System.Windows.RoutedEventArgs) =
        myColor <- MRed      
        enableParameterFields(true)
        () 
      
      override this.buttonRandomClick(sender: obj, 
                                      eArgs: System.Windows.RoutedEventArgs) =
        myColor <- MRandom
        enableParameterFields(false)
        () 
      
      override this.okButton_Click(sender: obj,
                                   eArgs: System.Windows.RoutedEventArgs) =
        // Only change the spirograph parameters in the model if we hit OK in the 
        // dialog box.
        if myColor = MRandom
        then myModel.Randomize
        else myR <- RBox.Text |> int
             myr <- rBox.Text |> int
             myl <- lBox.Text |> float
    
             myModel.MyR   <- myR
             myModel.Myr   <- myr
             myModel.Myl   <- myl
             model.MyColor <- myColor
    
        // Note that setting the DialogResult to nullable true is essential to get
        // the OK button to work.
        this.DialogResult <- new System.Nullable<bool> true         
        () 

Much of the code here is devoted to ensuring that the spirograph parameters in
Spirograph.fs match those shown in this dialog box. Note that there is no error
checking: If you enter a floating point for the integers expected in the top two
parameter fields, the program will crash. So, please add error checking in your
own effort.

Note also that the parameter input fields are disabled with Random color is picked
in the radio buttons. It's here just to show how it can be done.

In order to move data back and forth between the dialog box and the program I use
the System.Windows.Element.FindName() to find the appropriate control, cast it to
the control it should be, and then get the relevant settings from the Control.
Most other example programs use data binding. I did not for two reasons: First, I
couldn't figure out how to make it work, and second, when it didn't work I got no
error message of any kind. Maybe someone who visits this on StackOverflow can tell
me how to use data binding without including a whole new set of NuGet packages.



## Add the code behind for MainWindow.xaml
    namespace Spirograph
    
    type MainWindow(app: App, model: Model) as this =
      inherit MainWindowXaml()
    
      let myApp   = app
      let myModel = model
      
      let whenLoaded _ =
        ()
      
      let whenClosing _ =
        ()
      
      let whenClosed _ =
        () 
      
      let menuExitHandler _ = 
        System.Windows.MessageBox.Show("Good-bye", "Spirograph") |> ignore
        myApp.Shutdown()
        () 
      
      let menuParametersHandler _ = 
        let myParametersDialog = new DialogBox(myApp, myModel, this)
        myParametersDialog.Topmost <- true
        let bResult = myParametersDialog.ShowDialog()
        myModel.DrawSpirograph
        () 
      
      let menuDrawHandler _ = 
        if myModel.MyColor = MRandom then myModel.Randomize
        myModel.DrawSpirograph
        () 
      
      let menuAboutHandler _ = 
        System.Windows.MessageBox.Show("F#/WPF Menus & Dialogs", "Spirograph") 
        |> ignore
        () 
      
      do          
        this.Loaded.Add whenLoaded
        this.Closing.Add whenClosing
        this.Closed.Add whenClosed
        this.menuExit.Click.Add menuExitHandler
        this.menuParameters.Click.Add menuParametersHandler
        this.menuDraw.Click.Add menuDrawHandler
        this.menuAbout.Click.Add menuAboutHandler

There's not a lot going on here: We open the Parameters dialog box when required
and we have the option of redrawing the spirograph with whatever the current
parameters are.




## Add the App.xaml and App.xaml.fs to tie everything together
    <!-- All boilerplate for now -->
    <Application 
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"               
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml">      
        <Application.Resources>      
        </Application.Resources>  
    </Application>

Here's the code behind:

    namespace Spirograph
    
    open System  
    open System.Windows 
    open System.Windows.Controls
    
    module Main = 
      [<STAThread; EntryPoint>]
      let main _ =
        // Create the app and the model with the "business logic", then create the
        // main window and link its Canvas to the model so the model can access it.
        // The main window is linked to the app in the Run() command in the last line.
        let app = App()
        let model = new Model()
        let mainWindow = new MainWindow(app, model) 
        model.MyCanvas <- (mainWindow.FindName("myCanvas") :?> Canvas)         
        
        // Make sure the window is on top, and set its size to 2/3 of the dimensions 
        // of the screen.
        mainWindow.Topmost <- true
        mainWindow.Height  <- 
          (System.Windows.SystemParameters.PrimaryScreenHeight * 0.67)
        mainWindow.Width   <- 
          (System.Windows.SystemParameters.PrimaryScreenWidth * 0.67) 
        
        app.Run(mainWindow) // Returns application's exit code.

App.xaml is all boilerplate here, mainly to show where application resources, such
as icons, graphics, or external files - can be declared. The companion App.xaml.fs
pulls together the Model and the MainWindow, sizes the MainWindow to two-thirds of
the available screen size, and runs it.

When you build this, remember to make sure that the Build property for each xaml
file is set to Resource. Then you can either run through the debugger or compile
to an exe file. Note that you cannot run this using the F# interpreter: The
FsXaml package and the interpreter are incompatible.

There you have it. I hope you can use this as a starting point for your own
applications, and in doing so you can extend your knowlege beyond what is shown
here. Any comments and suggestions will be appreciated.


