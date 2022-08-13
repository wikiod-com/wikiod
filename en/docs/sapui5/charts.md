---
title: "Charts"
slug: "charts"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Applying filter on OData and Viz-Chart

This is an example of Viz-Charts with line-chart with filters. There are a lot of techniques this is one to solve the filtering issue.

Point to be noted is that you need to bind the Dataset of VizFrame by its ID and then apply the filtering on the FlattenedDataset  

In the controller:

        
    // defining the Filter
    var oFilter = new sap.ui.model.Filter("Data1",sap.ui.model.FilterOperator.GT,10);

    //Setting oModel
    var oModel = new sap.ui.model.odata.ODataModel("/destinations/v4/abc/http/app.svc", oConfig);
    this.getView().setModel(oModel);

    //Binding the filtered data to the chart by callind it from its ID and binding the data there
    this.getView().byId("idVizFrame").getDataset().getBinding("data").filter([oFilter]);
                        ﻿​

In the XML view:

    <viz:VizFrame id="idVizFrame" uiConfig="{applicationSet:'fiori'}" height='100%' width="100%" vizType='line' >    
        <viz:dataset>    
            <viz.data:FlattenedDataset data="{/YOUR_ENTITY_SET}">
                <viz.data:dimensions>
                    <viz.data:DimensionDefinition name="TimeStamp" value="{TimeStamp}"/>
                </viz.data:dimensions>
                <viz.data:measures>
                    <viz.data:MeasureDefinition name="SENSOR1" value="{SENSOR1}"/>
                </viz.data:measures>
            </viz.data:FlattenedDataset>
        </viz:dataset>
        <viz:feeds>
            <viz.feeds:FeedItem id='valueAxisFeed' uid="valueAxis" type="Measure" values="Data_SENSOR1"/>
            <viz.feeds:FeedItem id='categoryAxisFeed' uid="categoryAxis" type="Dimension" values="TimeStamp"/>
        </viz:feeds>
    </viz:VizFrame>

