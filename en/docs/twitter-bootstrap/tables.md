---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

**Content order and complex tables**
Beware that the table-reflow style changes the visual order of content. Make sure that you only apply this style to **well-formed** and simple data tables (and in particular, don’t use this for layout tables) with appropriate <th> table header cells for each row and column.

In addition, this class will not work correctly for tables with cells that span multiple rows or columns (using rowspan or colspan attributes).

## Basic table
Bootstrap defines a custom styling for table using the `.table` class. Just add the `.table` class to any `<table>` to see horizontal dividers and padding:

    <table class="table">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td>Bloggs</td></tr>
      </tbody>
    </table>

## Table with advanced styling
Bootstrap provides a couple of classes for advanced table styling.

# Striped rows
You will have a table with striped rows, if you add `.table-striped` class:
   
    <table class="table table-striped">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td>Bloggs</td></tr>
      </tbody>
    </table>

Note that:
> Striped tables are styled via the `:nth-child` CSS selector, which is not available in Internet Explorer 8.

# Bordered table
You will have a table with borders on all sides of the table and cells, if you add `.table-bordered` class:

    <table class="table table-bordered">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td>Bloggs</td></tr>
      </tbody>
    </table>

# Hover on rows
If you add `.table-hover` class, you will have a table with highlighted rows when the user hovers over a row:

    <table class="table table-hover">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td>Bloggs</td></tr>
      </tbody>
    </table>

# Condensed table
If you add `.table-condensed` class, the default cell padding will be cut in half, so you will have a more compact table:

    <table class="table table-hover">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td>Bloggs</td></tr>
      </tbody>
    </table>

# Contextual classes
Bootstrap tables support contextual colors. To change background color of a table row or cell you just have to add one of the following contexual classes: `.active`, `.success`, `.info`, `.warning`, `.danger`

    <table class="table">
      <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
      <tbody>
        <tr class="success"><td>John</td><td>Doe</td></tr>
        <tr><td>Fred</td><td class="info">Bloggs</td></tr>
      </tbody>
    </table>

## Responsive tables
You have to wrap any `.table` in html container with `.table-responsive` class to create responsive tables:

    <div class="table-responsive">
      <table class="table">
        <thead><tr><th>First Name</th><th>Last name</th></tr></thead>
        <tbody>
          <tr><td>John</td><td>Doe</td></tr>
          <tr><td>Fred</td><td>Bloggs</td></tr>
        </tbody>
      </table>
    </div>
Responsive tables will scroll horizontally on small devices (<768px). There will be no differences for screens larger than 768px wide.

## Table Reflow - Vertical headers
**Getting a table with vertical headers.**

Twitter bootstrap now support vertical header on a well formatted normal table. To achieve this just use `.table-reflow` class

Use twitter bootstrap `.table-reflow` class on a well formed table to achieve a table with vertical headers. Additionally you can combine with using `.table-striped` and `.table-hover` for hovering on columns this time.

    <table class="table table-striped table-hover table-reflow">
        <thead>
            <tr>
                <th ><strong> First Name: </strong></th>
                <th ><strong> Last Name: </strong></th>
                <th ><strong> Email: </strong></th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td> John </td>
                <td> Doe </td>
                <td> john.doe@email.com </td>
            </tr>
            <tr>
                <td> Joane </td>
                <td> Donald </td>
                <td> jane@email.com </td>
            </tr>
        </tbody>
    </table>

You should check the v4 alpha docs here:
[twitter-bootstrap .table-reflow][1]


  [1]: http://v4-alpha.getbootstrap.com/content/tables/#reflow

