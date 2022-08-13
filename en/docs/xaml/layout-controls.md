---
title: "Layout controls"
slug: "layout-controls"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Grid
`Grid` is used to create table layouts.

Basic rows and columns definitions
----------------------------------

    <Grid>
      <!-- Define 3 columns with width of 100 -->
      <Grid.ColumnDefinitions>
        <ColumnDefinition Width="100"/>
        <ColumnDefinition Width="100"/>
        <ColumnDefinition Width="100"/>
      </Grid.ColumnDefinitions>
      <!-- Define 3 rows with height of 50 -->
      <Grid.RowDefinitions>
        <RowDefinition Height="50"/>
        <RowDefinition Height="50"/>
        <RowDefinition Height="50"/>
      </Grid.RowDefinitions>
      <!-- This is placed at the top left (first row, first column) -->
      <Button 
          Grid.Column="0"
          Grid.Row="0"
          Content="Top Left"/>
      <!-- This is placed at the top left (first row, second column) -->
      <Button 
          Grid.Column="1"
          Grid.Row="0"
          Content="Top Center"/>
      <!-- This is placed at the center (second row, second column) -->
      <Button 
          Grid.Column="1"
          Grid.Row="1"
          Content="Center"/>
      <!-- This is placed at the bottom right (third row, third column) -->
      <Button 
          Grid.Column="2"
          Grid.Row="2"
          Content="Bottom Right"/>
    </Grid>


----------


*NOTE: All the following examples will use only columns, but are applicable to rows as well*.


----------

Auto size definitions
---------------------

Columns and rows can be defined with "Auto" as their width/height. Auto size will take *as   much space as it needs* to display its content, and no more.  
Auto sized definitions can be used with fixed size definitions.

    <Grid>
      <Grid.ColumnDefinitions>
        <ColumnDefinition Width="Auto"/>
        <ColumnDefinition Width="Auto"/>
        <ColumnDefinition Width="50"/>
      </Grid.ColumnDefinitions>
      <!-- This column won't take much space -->
      <Button Grid.Column="0" Content="Small"/>
      <!-- This column will take much more space -->
      <Button Grid.Column="1" Content="This text will be very long."/>
      <!-- This column will take exactly 50 px -->
      <Button Grid.Column="2" Content="This text will be cut"/>
    </Grid>


----------

Simple star sized definitions
-----------------------------

Columns and rows can be defined with `*` as their width/height. Star sized rows/columns will take *as much space as it has*, regardless of it's content.  
Star sized definitions can be used with fixed and auto sized definitions.
Star size is the default and thus the column width or row height can be omitted.

    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition Width="*"/>
            <ColumnDefinition Width="50"/>
        </Grid.ColumnDefinitions>
        <!-- This column will be as wide as it can -->
        <Button Grid.Column="0" Content="Small"/>
        <!-- This column will take exactly 50 px -->
        <Button Grid.Column="2" Content="This text will be cut"/>
    </Grid>


----------

Proportional star sized definitions
-----------------------------------

Besides the fact that star takes as much space as it can, star definitions are also proportional to each other. If nothing else is mentioned, each star definition will take as much space as the others in the current grid.

However, it is possible to define a ratio between the sizes of different definitions by simply adding a multiplier to it. So a column defined as `2*` will be twice as wide as a column defined as `*`. The width of a single unit is calculated by dividing the available space by the sum of the multipliers (if there's non it's counted as 1).  
So a grid with 3 columns defined as `*`, `2*`, `*` will be presented as 1/4, 1/2, 1/4.  
And one with 2 columns defined as `2*`, `3*` will be presented 2/5, 3/5.

If there are auto or fixed definitions in the set, these will be calculated first, and the star definitions will take the remaining space after that.

    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition Width="*"/>
            <ColumnDefinition Width="2*"/>
            <ColumnDefinition Width="*"/>
        </Grid.ColumnDefinitions>
        <!-- This column will be as wide as the third column -->
        <Button Grid.Column="0" Content="Small"/>
        <!-- This column will be twice as wide as the rest -->
        <Button Grid.Column="1" Content="This text will be very long."/>
        <!-- This column will be as wide as the first column -->
        <Button Grid.Column="2" Content="This text will may be cut"/>
    </Grid>


----------

Column/Row Span
---------------

It's possible to make a control stretch beyond it's cell by setting it's Row/ColumnSpan. The value set is the number of rows/columns th

    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition Width="*"/>
            <ColumnDefinition Width="2*"/>
            <ColumnDefinition Width="*"/>
        </Grid.ColumnDefinitions>
        <!-- This control will streach across most of the grid -->
        <Button Grid.Column="0" Grid.ColumnSpan="2" Content="Small"/>
        <Button Grid.Column="2" Content="This text will may be cut"/>
    </Grid>

## Canvas
`Canvas` is the simplest of panels. It places items at the specified `Top/Left` coordinates.

    <Canvas>
      <TextBlock 
        Canvas.Top="50" 
        Canvas.Left="50" 
        Text="This is located at 50, 50"/>
      <TextBlock 
        Canvas.Top="100"
        Canvas.Left="50"
        Width="150"
        Height="23"
        Text="This is located at 50, 100 with height 23 and width 150"/>
    </Canvas>



## DockPanel
`DockPanel` aligns the control according to the dock property, in the order it's placed in the control.

NOTE: `DockPanel` is part of the WPF framework, but does not come with Silverlight/WinRT/UWP. Open-source implementations are easy to find though.

    <DockPanel LastChildFill="False">
      <!-- This will strech along the top of the panel -->
      <Button DockPanel.Dock="Top" Content="Top"/>
      <!-- This will strech along the bottom of the panel -->
      <Button DockPanel.Dock="Bottom" Content="Bottom"/>
      <!-- This will strech along the remaining space in the left of the panel -->
      <Button DockPanel.Dock="Left" Content="Left"/>
      <!-- This will strech along the remaining space in the right of the panel -->
      <Button DockPanel.Dock="Right" Content="Right"/>
      <!-- Since LastChildFill is false, this will be placed at the panel's right, to the left of the last button-->
      <Button DockPanel.Dock="Right" Content="Right"/>
    </DockPanel>


----------


    <!-- When lastChildFill is true, the last control in the panel will fill the remaining space, no matter what Dock was set to it -->
    <DockPanel LastChildFill="True">
      <!-- This will strech along the top of the panel -->
      <Button DockPanel.Dock="Top" Content="Top"/>
      <!-- This will strech along the bottom of the panel -->
      <Button DockPanel.Dock="Bottom" Content="Bottom"/>
      <!-- This will strech along the remaining space in the left of the panel -->
      <Button DockPanel.Dock="Left" Content="Left"/>
      <!-- This will strech along the remaining space in the right of the panel -->
      <Button DockPanel.Dock="Right" Content="Right"/>
      <!-- Since LastChildFill is true, this will fill the remaining space-->
      <Button DockPanel.Dock="Right" Content="Fill"/>
    </DockPanel>





## StackPanel
StackPanel places its controls one after another. It acts like a dock panel with all of its control's docks set to the same value.

    <!-- The default StackPanel is oriented vertically, so the controls will be presented in order from top to bottom -->
    <StackPanel>
      <Button Content="First"/>
      <Button Content="Second"/>
      <Button Content="Third"/>
      <Button Content="Fourth"/>
    </StackPanel>


----------


    <!-- Setting the Orientation property to Horizontal will display the control in order from left to right (or right to left, according to the FlowDirection property) -->
    <StackPanel Orientation="Horizontal">
      <Button Content="First"/>
      <Button Content="Second"/>
      <Button Content="Third"/>
      <Button Content="Fourth"/>
    </StackPanel>

To stack items from the bottom up, use a dock panel.

## WrapPanel
The wrap panel acts in a similar way to stack panel. Except when it recognizes the items will exceed it's size, it would then wrap them to a new row/column, depending on it's orientation.

---

## Horizontal orientation ##

    <WrapPanel Width="100">
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
    </WrapPanel>

---

## Vertical wrap panel ##

    <WrapPanel Height="70" Orientation="Vertical">
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
    </WrapPanel>


 

## UniformGrid
Uniform grid will place all it's children in a grid layout, each child in it's own cell. All the cells will have the same size. It can be thought to be a shorthand to a grid where all the row and column definitions are set to `*`

---

## Default rows and columns ##

By default the UniformGrid will try to create an equal number of rows and columns. When a row will become to long, it will add a new column.

This code will produce a grid of 3x3 with the first 2 rows filled and the last with one button:

    <UniformGrid>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
    </UniformGrid>

----------

## Specified rows / columns ##

You can tell the UniformGrid exactly how many rows and/or column you wish to have.  

    <UniformGrid Columns="2" >
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
    </UniformGrid>

*NOTE: in case both rows and columns are set, and there are more children than cells, the last children in the grid won't be displayed*

---

## FirstColumn Property ##

Once the Columns property is set, you can set the FirstColumn property. This property will enter x empty cells to the first row before the first child is displayed.
FirstColumn must be set to a number smaller than the Columns property.

In this example the first button will be displayed in the first row's second column:

    <UniformGrid Columns="2" FirstColumn="1">
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
        <Button Content="Button"/>
    </UniformGrid>



## RelativePanel
[`RelativePanel`](https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.relativepanel.aspx) has been introduced in Windows 10 and is used mainly to support adaptive layouts, where the child elements of the panel are laid out differently depending on available space.
`RelativePanel` is typically used with [visual states](https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.visualstate.aspx), which are used to switch the layout configuration, adapting to the screen or window size, orientation or use case.
The child elements use attached properties that define where they are in relation to the panel and each other.

    <RelativePanel
        VerticalAlignment="Stretch"
        HorizontalAlignment="Stretch">
        <Rectangle
            x:Name="rectangle1"
            RelativePanel.AlignLeftWithPanel="True"
            Width="360"
            Height="50"
            Fill="Red"/>
        <Rectangle
            x:Name="rectangle2"
            RelativePanel.Below="rectangle1"
            Width="360"
            Height="50"
            Fill="Green" />
    </RelativePanel>
    <VisualStateManager.VisualStateGroups>
        <VisualStateGroup>
            <VisualState>
                <VisualState.StateTriggers>
                    <!--VisualState to be triggered when window width is >=720 effective pixels.-->
                    <AdaptiveTrigger
                        MinWindowWidth="720" />
                </VisualState.StateTriggers>
                <VisualState.Setters>
                    <Setter
                        Target="rectangle2.(RelativePanel.Below)"
                        Value="{x:Null}" />
                    <Setter
                        Target="rectangle2.(RelativePanel.RightOf)"
                        Value="rectangle1" />
                    <Setter
                        Target="rectangle1.(RelativePanel.AlignLeftWithPanel)"
                        Value="False" />
                    <Setter
                        Target="rectangle1.(RelativePanel.AlignVerticalCenterWithPanel)"
                        Value="True" />
                    <Setter
                        Target="rectangle2.(RelativePanel.AlignVerticalCenterWithPanel)"
                        Value="True" />
                </VisualState.Setters>
            </VisualState>
        </VisualStateGroup>
    </VisualStateManager.VisualStateGroups>


