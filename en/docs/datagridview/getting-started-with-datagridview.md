---
title: "Getting started with datagridview"
slug: "getting-started-with-datagridview"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction to DataGridView
A `DataGridView` is a control in .NET UI design, which consists of rows and columns used to arrange data.

Often there is need to depict data either from a spreadsheet or database on a UI design in an application. When this data is to be shown grouped by its properties, we choose a `DataGridView`.

In C# Window Forms, a `DataGridView` can be instantiated using a statement like:

    DataGridView dgv = new DataGridview();

A column count can be used to state beforehand as to how many columns are present in a `DataGridView` like so:

    dgv.ColumnCount = 4;


A `DataGridView` consists of data categorized by columns. The column header is typically used to show what data the `DataGridView` Contains.


        dgv.Columns[0].Name = "Name";
        dgv.Columns[1].Name = "Age";
        dgv.Columns[2].Name = "Nationality";
        dgv.Columns[3].Name = "Height";



