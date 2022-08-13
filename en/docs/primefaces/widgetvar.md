---
title: "widgetVar"
slug: "widgetvar"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

**widgetVar** is the name of the client side variables which contains all the javascript PF widgets on the page.  There is a great intro/tutorial to using the widgetVar component written by Hatem Alimam called [Intro To PrimeFaces widgetVar][1]


  [1]: http://blog.hatemalimam.com/intro-to-primefaces-widgetvar/

## Basic usage of widgetVar
    <h:form>
        <p:dialog widgetVar="myDialog"></p:dialog>
        <p:commandButton onclick="PF('myDialog').show();" />
    </h:form>

## Datatable
[`datatable.js`][datatable] in GitHub Reporitory

| Function| Details |  
| --------- | ------- |  
| `bindPaginator: function()` | Binds the change event listener and renders the paginator | 
| `loadLiveRows: function()` | Loads rows on-the-fly when scrolling live |
| `paginate: function(newState)` | Ajax pagination |
| `fetchNextPage: function(newState)` | Loads next page asynchronously to keep it at viewstate and Updates viewstate |
| `sort: function(columnHeader, order, multi)` | Ajax sort |
| `filter: function()` | Ajax filter |
| `onRowClick: function(event, rowElement, silent)` | |
| `onRowDblclick: function(event, row)` | |
| `highlightRow: function(row)` | Highlights row as selected |
| `unhighlightRow: function(row)` | Clears selected visuals |
| `fireRowSelectEvent: function(rowKey, behaviorEvent)` | Sends a rowSelectEvent on server side to invoke a rowSelectListener if defined |
| `fireRowUnselectEvent: function(rowKey, behaviorEvent)`| Sends a rowUnselectEvent on server side to invoke a rowUnselectListener if defined |
| `selectRowWithRadio: function(radio)` | Selects the corresping row of a radio based column selection |
| `unselectAllRows: function()` | |
| `selectAllRowsOnPage: function()` | |
| `unselectAllRowsOnPage: function()` | |
| `selectAllRows: function()` | |
| `toggleExpansion: function(toggler)` | Expands a row to display detail content |
| `collapseRow: function(row)` | |
| `collapseAllRows: function()` | |
| `getExpandedRows: function()` | |
| `switchToRowEdit: function(row)` | |
| `showRowEditors: function(row)` | |
| `saveRowEdit: function(rowEditor)` | Saves the edited row | 
| `cancelRowEdit: function(rowEditor)` | |
| `updateRow: function(row, content)` | Updates row with given content | 
| `clearSelection: function()` | Clears the selection state |
| `clearFilters: function()` | Clears table filters |
| `removeSelection: function(rowIndex)` | Remove given rowIndex from selection |
| `addSelection: function(rowKey)` | Adds given rowKey to selection if it doesn't exist already |
| `isSelected: function(rowKey)` | Finds if given rowKey is in selection | 
| `saveColumnOrder: function()` | |
| `isEmpty: function()` | Returns if there is any data displayed |
| `getSelectedRowsCount: function()` | |
| 


  [datatable]: https://github.com/primefaces/primefaces/blob/master/src/main/resources/META-INF/resources/primefaces/datatable/datatable.js

