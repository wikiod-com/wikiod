---
title: "Databinding"
slug: "databinding"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Parameters
| Argument | Description |
| ------ | ------ |
| propertyName | The name of the control property to bind. |
| dataSource | An Object representing the data source. |
| dataMember | The property or list to bind to. |
| formattingEnabled | Determines, whether the displayed data should be formatted. |
| updateMode | Data source is updated when the control property is validated (default), or immediately when the property has changed |
| nullValue | When the data source has this value, the bound property is set to DBNull. |
| formatString | One or more format specifier characters that indicate how a value is to be displayed |
| formatInfo | An implementation of IFormatProvider to override default formatting behavior. |

See https://msdn.microsoft.com/en-us/library/ef2xyb33.aspx
Databinding only works with properties, never with fields!

## Binding controls to data objects
Each control has a property `DataBindings` which is a list of `System.Windows.Forms.Binding` objects. The Add()-method has some overloads which enables you easily binding to the property of an object:

    textBox.DataBindings.Add( "Text", dataObj, "MyProperty" );
Note, that binding basically means subscribing to each others changeevent. The code above subscribes to changeevent of dataObj.MyProperty and adapts textBox.Text when it changes. And vice versa it subscribes to textBox.TextChanged and adapts dataObj.MyPropery when it changes.

