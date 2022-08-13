---
title: "Migration from <1.10 to 1.10 and above"
slug: "migration-from-110-to-110-and-above"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Datatables 1.10.x is the latest release as of now. Though it is backwards compatible to the previous versions (1.9 etc.), it is highly advisable to use the latest version which directly returns a `datatable api` object.

Another major change, that is the most visible, is the change from [Hungarian Notation](https://en.wikipedia.org/wiki/Hungarian_notation) to [camelCase](https://en.wikipedia.org/wiki/Camel_case)



## Syntax
 - The only major syntax change is the usage of camelCase everywhere
   instead of the Hungarian Notation.
 - A more [detailed guide can be
   found here](https://datatables.net/upgrade/1.10-convert) which
   involves the conversion of parameters in <1.10 to 1.10+

For more details, try visiting the following pages:

1. [Upgrading to Datatables 1.10](https://datatables.net/upgrade/1.10)
2. [Changelog Datatables 1.10](https://datatables.net/new/1.10)
3. [Convert Parameter Names from 1.9 to 1.10](https://datatables.net/upgrade/1.10-convert)
4. [Using the Datatable API](https://datatables.net/manual/api)

## Initialisation of Datatable 1.10+
Previously, datatables were initialized as follows:

    var oTable = $("#selector").dataTable();

This used to return a jQuery object which would stored in the variable oTable. And then to access the api to modify table properties, we had to initialize the api differently, as shown below:

    var api = oTable.api()
    //r
    var api = new $.fn.dataTable.api("#selector")

Now, the initialization is changed to the following:

    var table = $("#selector").DataTable();

Note that this returns a `datatable api` instance and you can directly use variable `table` to manipulate datatable properties.

## Features Not available in Datatables 1.10+
The following 3 features (that were deprecated in 1.9) are no longer available in 1.10, they are :

 1. **fnRender**: According to the deverloper:

> The old fnRender option provided a method of manipulating a cell when
> it was created. however, it was provided with a confusing list of
> options as its arguments, and required a particular structure in
> DataTables internally that caused performance issues. Removal of
> fnRender has lead to a significant improvement in performance of
> DataTables with large data sets and the ability to provide object
> instances to DataTables as data source objects (for example Knockout
> observable objects).

Alternatives to `fnRender` are available as [`columns.render`](https://datatables.net/reference/option/columns.renderhttps://datatables.net/reference/option/columns.render) and [`columns.createdCell`](https://datatables.net/reference/option/columns.createdCell)

 2. **bScrollInfinite**: According to the developer:

> The built-in ability of DataTables 1.9 to show an infinitely scrolling
> grid through the bScrollInfinite option has been removed due to the
> inconsistencies it caused in the API. Removal has also helped simply
> the internal code significantly.

An extension that goes by the name of [`Scroller`](https://datatables.net/extensions/scroller/) is available as an alternative.

3. **Cookie based state saving**: 

> Cookie based state saving has been replaced with localStorage based
> state saving in DataTables 1.10. Cookie's, with their 4KiB limit were
> very limited, and incurred a performance penalty since they were part
> of every HTTP request. localStorage is much faster and more flexible,
> and is used as the default storage for state information in DataTables
> 1.10.

4. **`two_button` pagination control**:

> DataTables 1.10 has significantly upgraded the paging controls of DataTables (see [pagingType](https://datatables.net/reference/option/pagingType)), a consequence of which is that the old built-in two_button form of paging has been removed.

They have taken care of people who still want to use the `two_button` pagination method by providing an extra javascript file called [`two_button.js`](https://datatables.net/upgrade/1.10_two_button.js). Usage is as follows:

> Simply include this file in your document, after you load DataTables but before you initialise your table and the two_button pagination will be restored exactly as it was in 1.9 (including class names etc).






