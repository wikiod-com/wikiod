---
title: "sapui5 Table"
slug: "sapui5-table"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
 

 1. var oTable = new selectAllVisibleRowsTable({...
//alternativelty can use new sap.ui.table.Table
 2. sap.ui.table.Table.extend('selectAllVisibleRowsTable', { ..... 
// here table name is in quotes 


Official documentation for sapui5 table that provides only API specification. 
https://sapui5.hana.ondemand.com/docs/api/symbols/sap.ui.table.Table.html



## SAPUI5 Responsive Table
A Responsive Table(sap.m.Table) can be created as below

**XML View**

    <mvc:View
    controllerName="com.sap.app.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns:core="sap.ui.core"
    xmlns="sap.m">
    <Page title="Table Example">
        <content>
        <Table id="idEmployeesTable"
            items="{/Employees}">
            <headerToolbar>
                <Toolbar>
                    <Title text="Employees"/>
                </Toolbar>
            </headerToolbar>
            <columns>
                <Column>
                    <Text text="Name" />
                </Column>
                <Column>
                    <Text text="City" />
                </Column>
                <Column>
                    <Text text="Country" />
                </Column>
                <Column
                    hAlign="Right">
                    <Text text="Reporting" />
                </Column>
            </columns>
            <items>
                <ColumnListItem>
                    <cells>
                        <Text
                            text="{FirstName} {LastName}" />
                        <Text
                            text="{City}" />
                        <Text
                            text="{Country}" />
                        <ObjectNumber
                            number="{ReportsTo}"
                            unit="employees"
                            />                      
                    </cells>
                </ColumnListItem>
            </items>
        </Table>
        </content>
    </Page>
    </mvc:View>

**Controller JS**

    var oModel = new sap.ui.model.odata.ODataModel("http://services.odata.org/V2/Northwind/Northwind.svc");
    this.getView().setModel(oModel);

## Sample table for sapui5 with control and processing examples
   
    //Create a layout
     var tableLayout = new sap.ui.commons.layout.MatrixLayout({
        layoutFixed : false,
        columns : 2,
        width : "100%",
        height : "100%",
        widths : [ "20%","80%"]
    }).addStyleClass('dsAvailLayout');

    sap.ui.table.Table.extend('selectAllVisibleRowsTable', {
        renderer : function(oRm, oControl) {
            sap.ui.table.TableRenderer.render(oRm, oControl);
        },

        selectAllVisibleRowsIndex: function(checkKey) {
            var model = this.getModel();
            var rowPath = this.getBindingInfo('rows').path;
            var rows = model.getProperty(rowPath);
            var start = this.getFirstVisibleRow();
            var end = Math.min(start + this.getVisibleRowCount(), rows.length);
            
            for (var i = 0; i < rows.length; i++) {
                var row = rows[i];
                row[checkKey] = (i >= start && i < end);
            }
            this.invalidate();
        },
        selectAll: function(checkKey) {
            var model = this.getModel();
            var rowPath = this.getBindingInfo('rows').path;
            var rows = model.getProperty(rowPath);
            var start = this.getFirstVisibleRow();
            var end = rows.length;
            
            for (var i = 0; i < rows.length; i++) {
                var row = rows[i];
                row[checkKey] = (i >= start && i < end);
            }
            this.invalidate();
        },
        handle: function(){
            try{
                var model = this.getModel();
                var rowPath = this.getBindingInfo('rows').path;
                var rows = model.getProperty(rowPath);
                var selectedIndices = [];
                for (var i = 0; i < rows.length; i++) { 
                    var row = rows[i];
                    if(row['checked'] == true){
                        selectedIndices.push(i);
                    }
                }
                
                objStr = "";
                var suffix = "";
                for (var i = 0; i < selectedIndices.length; i++) {
                    var idx = selectedIndices[i];
                      var cxt = this.getContextByIndex(idx);
                      var path = cxt.sPath;
                      var obj = this.getModel().getProperty(path);
                      objStr = objStr+suffix+JSON.stringify(obj);
                      suffix = ",";
                  }
            }catch(err){
                    
            }
        }
    });

    var oTable = new selectAllVisibleRowsTable({
        width: '100%',
        selectionMode : sap.ui.table.SelectionMode.None,
        rowSelectionChange: function(e) {
          var indices = e.getParameter('rowIndices');
          for (var i = 0; i < indices.length; i++) {
            var idx = indices[i];
            if (oTable.isIndexSelected(idx)) {
              var cxt = oTable.getContextByIndex(idx);
              var path = cxt.sPath;
              var obj = oTable.getModel().getProperty(path);
              //console.log(JSON.stringify(obj)); 
              alert(JSON.stringify(obj));
            }
          }
        },
        columns:[new sap.ui.table.Column({
            label: '',
            width: '5%',
            template: new sap.ui.commons.CheckBox({
                checked: '{checked}'
            })
        }),
         new sap.ui.table.Column({
            label: new sap.ui.commons.TextView({  
                text: "Property"  
            }),  
            width: '60%',
            disabled:true,
            template: new sap.ui.commons.TextView({
                text: '{property}'
            })
        }),
        new sap.ui.table.Column({
            label: new sap.ui.commons.TextView({  
                text: "Type"  
            }),  
            width: '35%',
            template: new sap.ui.commons.TextView({
                text: '{type}'
            })
        })
         
        ]  
        
    });

    var oTableLbl = new sap.ui.commons.Label({
        text : "Select Property:",
        labelFor : oTable
    });
    
    tableLayout.createRow({
        height : "70px"
    }, oTableLbl,oTable);

    tableLayout.createRow({
        height : "30px"
    }, "" ,(new sap.ui.commons.Button({
        text: 'Select visible',
        press: function(e) {
            oTable.selectAllVisibleRowsIndex('checked');
        }
    })));
  

      
        tableLayout.createRow({
            height : "30px"
        }, "" ,(new sap.ui.commons.Button({
            text: 'Select All',
            press: function(e) {
                oTable.selectAll('checked');
            }
        })));
    
     tableLayout.createRow({
            height : "30px"
        }, "" ,(new sap.ui.commons.Button({
            text: 'OK,
            press: function(e) {
               oTable.bindRows('/');
                            var model = new sap.ui.model.json.JSONModel();
                            entityResults = JSON.parse(response.replace("meta", ""));
                            isErrorExists = false;
                            var data = [];
                            for ( var key in entityResults) {
                                if (entityResults.hasOwnProperty(key)) {
                                    data.push({
                                            property : entityResults[key].name, 
                                            type :  entityResults[key].type,
                                            filter : entityResults[key].filter,
                                            checked : false
                                        });
                                }
                            }
                            model.setData(data);
                            oTable.setModel(model);
            }
        })));



