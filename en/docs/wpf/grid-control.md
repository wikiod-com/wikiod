---
title: "Grid control"
slug: "grid-control"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## A simple Grid
    <Grid>
        <Grid.RowDefinitions>
            <RowDefinition/>
            <RowDefinition/>
        </Grid.RowDefinitions>
        <Grid.ColumnDefinitions>
            <ColumnDefinition/>
            <ColumnDefinition/>
        </Grid.ColumnDefinitions>
        <TextBlock Grid.Column="1" Text="abc"/>
        <TextBlock Grid.Row="1" Grid.Column="1" Text="def"/>
    </Grid>

Rows and columns are defined adding `RowDefinition` and `ColumnDefinition` elements to the corresponding collections.

There can be an abitrary amount of children in the `Grid`. To specify which row or column a child is to be placed in the attached properties `Grid.Row` and `Grid.Column` are used. Row and column numbers are zero based. If no row or column is set it defaults to `0`.

Children placed in the same row and column are drawn in order of definition. So the child defined last will be draw above the child defined before.

## Syncing rows or columns of multiple Grids
The row heigths or column widths of multiple `Grid`s can be synchronized by setting a common `SharedSizeGroup` on the rows or columns to synchronize. Then a parent control somewhere up in the tree above the `Grid`s needs to have the attached property `Grid.IsSharedSizeScope` set to `True`.

    <StackPanel Grid.IsSharedSizeScope="True">
        <Grid>
            <Grid.ColumnDefinitions>
                <ColumnDefinition Width="Auto" SharedSizeGroup="MyGroup"/>
                <ColumnDefinition Width="*" />
            </Grid.ColumnDefinitions>
            [...]
        </Grid>
        <Grid>
            <Grid.ColumnDefinitions>
                <ColumnDefinition Width="Auto" SharedSizeGroup="MyGroup"/>
                <ColumnDefinition Width="*" />
            </Grid.ColumnDefinitions>
            [...]
        </Grid>
    </StackPanel>

In this example the first column of both `Grid`s will always have the same width, also when one of them is resized by its content.

## Grid children spanning multiple rows/columns
By using the `Grid.RowSpan` and `Grid.ColumnSpan` attached properties, children of a `Grid` can span multiple rows or columns.
In the following example the second `TextBlock` will span the second and third column of the `Grid`.
  

   

    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition/>
            <ColumnDefinition/>
            <ColumnDefinition/>
        </Grid.ColumnDefinitions>
        <TextBlock Grid.Column="2" Text="abc"/>
        <TextBlock Grid.Column="1" Grid.ColumnSpan="2" Text="def"/>
    </Grid>

