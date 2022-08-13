---
title: "Aggregation binding"
slug: "aggregation-binding"
draft: false
images: []
weight: 9860
type: docs
toc: true
---

## Parameters
| Parameter | Detail |
| ------ | ------ |
| path   | Path of the object or list of objects that will be included in the aggregation. |
| factory| Function that will create the view element of the aggregation. |
| sorter | Object that represents the way that the aggregation objects will be sorted.|





## Aggregation binding using templates in xmlview
XmlView:   

 
    <mvc:View
        controllerName="sap.m.sample.ListCounter.List"
        xmlns:mvc="sap.ui.core.mvc"
        xmlns="sap.m">
        <List
            headerText="Products"
            items="{products>/Products}">
            <!-- Template of the list item -->
            <StandardListItem
                title="{Name}"
            />
        </List>
    </mvc:View>

Controller:

    sap.ui.define([
            'jquery.sap.global',
            'sap/ui/core/mvc/Controller',
            'sap/ui/model/json/JSONModel'
        ], function(jQuery, Controller, JSONModel) {
        "use strict";
     
        var ListController = Controller.extend("sap.m.sample.ListCounter.List", {
     
            onInit : function (evt) {
                // Model
                var oModel = new JSONModel("/products.json"));
                this.getView().setModel(oModel,"products");
            }
        });
     
     
        return ListController;
     
    });

products.json:

    { 
        Products : [
            {"Name": "Product 1"},
            {"Name": "Product 2"},
            {"Name": "Product 3"},
            ]
    }

## Aggregation binding with sorting and static filters
    <mvc:View
        controllerName="sap.m.sample.ListCounter.List"
        xmlns:mvc="sap.ui.core.mvc"
        xmlns="sap.m">
        <List
            headerText="Fruits"
            items="{path:'products>/Products', sorter:{path:'Name'}, filter:{path:'Type', operator:'EQ',value1:'Fruit'}}">
            <!-- Template of the list item -->
            <StandardListItem
                title="{Name}"
            />
        </List>
        <List
            headerText="Food"
            items="{path:'products>/Products', sorter:{path:'Name'}, filter:{path:'Type', operator:'EQ',value1:'Food'}}">
            <!-- Template of the list item -->
            <StandardListItem
                title="{Name}"
            />
        </List>
    </mvc:View>

Controller:

    sap.ui.define([
            'jquery.sap.global',
            'sap/ui/core/mvc/Controller',
            'sap/ui/model/json/JSONModel'
        ], function(jQuery, Controller, JSONModel) {
        "use strict";
     
        var ListController = Controller.extend("sap.m.sample.ListCounter.List", {
     
            onInit : function (evt) {
                // Model
                var oModel = new JSONModel("/products.json"));
                this.getView().setModel(oModel,"products");
            }
        });
     
     
        return ListController;
     
    });

products.json:

    { 
        Products : [
            {"Name": "Banana", "Type": "Fruit"},
            {"Name": "Meat", "Type":"Food"},
            {"Name": "Apple", "Type": "Fruit"},
            {"Name": "Rice", "Type": "Food"},
            ]
    }

## Aggregation Binding with Factory Function
XmlView:

    <mvc:View
        controllerName="sap.ui.demo.wt.controller.App"
        xmlns="sap.m"
        xmlns:mvc="sap.ui.core.mvc"
        displayBlock="true">
        <App>
            <pages>
                <Page content="{path:'Tiles>/Tiles',factory:'.tileFactory'}">
                    
                </Page>
            </pages>
        </App>
    </mvc:View>

Controller:

    sap.ui.define([
        "sap/ui/core/mvc/Controller",
        "sap/ui/model/json/JSONModel"
    ], function (Controller, JSONModel) {
        "use strict";
    
        return Controller.extend("sap.ui.demo.wt.controller.App", {
    
            onInit: function(){
                var oModel  = new JSONModel("./model/data.json");
                
                this.getView().setModel(oModel,"Tiles");
            },
            tileFactory: function(sId,oContext){
                var oUIControl = null;
                
                var type = oContext.getProperty("type");
                
                switch(type){
                    case "STD":
                        var title = oContext.getProperty("Title");
                        oUIControl = new sap.m.StandardTile();
                        
                        oUIControl.setTitle(title);
                        break;
                    case "NEWS":
                        var title = oContext.getProperty("Title");
                        var newsContent = new sap.m.NewsContent({contentText:title});
                        oUIControl = new sap.m.GenericTile();
                        oUIControl.addTileContent(new sap.m.TileContent({content:newsContent}));
                        break;
                    case "IMG":
                        var src = oContext.getProperty("src");
                        var imgContent = new sap.m.ImageContent({src});
                        oUIControl = new sap.m.GenericTile();
                        oUIControl.addTileContent(new sap.m.TileContent({content:imgContent}));
                        break;
                }
                
                return oUIControl;
            
            }
        });
    
    });

data.json:

    {
        "Tiles":[
            {
            "type": "STD",
            "Title": "Standard Tile"
            },
            {
            "type": "NEWS",
            "Title": "NEWS Tile"
            },
            {
            "type": "IMG",
            "src": "https://1.bp.blogspot.com/-2YLGmdxqXMk/V58ki-s5DLI/AAAAAAAANhs/jcSRMEeJN_89vXNdrie1jDGFhF5X-yh4ACLcB/s1600/ui5.png"
            }
            ]
    }

