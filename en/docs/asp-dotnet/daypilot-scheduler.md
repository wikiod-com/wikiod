---
title: "DayPilot Scheduler"
slug: "daypilot-scheduler"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Parameters
| Parameter| Desc |
| ------ | ------ |
| DataStartField| specifies the data source column that contains event start (DateTime) |
| DataStartField   |  specifies the data source column that contains event start (DateTime)  |
| DataEndField   | specifies the data source column that contains event end (DateTime)   |
| DataTextField   |  specifies the data soruce column that contains event text (string)  |
|  DataIdField  |  specifies the data source column that contains event id (string or integer)  |
|  DataResourceField  | specifies the data soruce column that contains event resource foreign key (string)   |



This is  basics of DayPilot schedular which needs to be further explore.

## Basic Info
DayPilot Scheduler widget displays a time line for multiple resources. Supports AJAX and HTML5. Automatic and manual localization. Full CSS styling support

## Declaration
    <%@ Register Assembly="DayPilot" Namespace="DayPilot.Web.Ui" TagPrefix="DayPilot" %>
     <DayPilot:DayPilotScheduler 
     ID="DayPilotScheduler1" 
     runat="server" 

     DataStartField="eventstart" 
     DataEndField="eventend" 
     DataTextField="name" 
     DataIdField="id" 
     DataResourceField="resource_id" 

     CellGroupBy="Month"
     Scale="Day"

     EventMoveHandling="CallBack" 
     OnEventMove="DayPilotScheduler1_EventMove"  >
   
    </DayPilot:DayPilotScheduler>

