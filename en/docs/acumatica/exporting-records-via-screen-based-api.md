---
title: "Exporting Records via Screen-Based API"
slug: "exporting-records-via-screen-based-api"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This topic will demonstrate how to export records from Acumatica ERP via the Screen-Based API. The Screen-Based API of Acumatica ERP provides only the SOAP interface. If your development platform has limited support for SOAP web services, consider the Contract-Based API providing both SOAP and REST interfaces. For more information on the Screen-Based API, see [Acumatica ERP Documentation][1]

  [1]: https://docref.acumatica.com/wiki/ShowWiki.aspx?pageid=c8806b6b-af5c-4c29-9112-b611bd08257e

All sample provided in this topic were created with the Screen-Based API Wrapper. If you want your client application to not depend on the UI changes in the Acumatica ERP application, you should use the screen-based API wrapper, which is described in [Acumatica ERP Documentation][1]

  [1]: https://docref.acumatica.com/Wiki/ShowWiki.aspx?pageid=37c6db0a-8233-4f07-b496-d84a0e00fcfc

## Data Export from an Entry Form with a Single Primary Key
The **Stock Items** screen (IN.20.25.00) is one of the most often used data entry forms of Acumatica ERP to export data. ***Inventory ID*** is the only primary key on the **Stock Items** screen:
[![enter image description here][1]][1]

To export records from a data entry form, your SOAP request must always begin with the `ServiceCommands.Every[Key]` command, where `[Key]` is to be replaced with primary key name. 

# To export all stock items in a single web service call: #

    Screen context = new Screen();
    context.CookieContainer = new System.Net.CookieContainer();
    context.Url = "http://localhost/AcumaticaERP/Soap/IN202500.asmx";
    context.Login(username, password);
    try
    {
        Content stockItemsSchema = PX.Soap.Helper.GetSchema<Content>(context);
        Field lastModifiedField = new Field
        {
            ObjectName = stockItemsSchema.StockItemSummary.InventoryID.ObjectName,
            FieldName = "LastModifiedDateTime"
        };
        var commands = new Command[]
        {
            stockItemsSchema.StockItemSummary.ServiceCommands.EveryInventoryID,
            stockItemsSchema.StockItemSummary.InventoryID,
            stockItemsSchema.StockItemSummary.Description,
            stockItemsSchema.GeneralSettingsItemDefaults.ItemClass,
            stockItemsSchema.GeneralSettingsUnitOfMeasureBaseUnit.BaseUnit,
            lastModifiedField
        };
        var items = context.Export(commands, null, 0, false, false);
    }
    finally
    {
        context.Logout();
    }

With time amount of data in any ERP application tends to grow in size. If you will be exporting all records from your Acumatica ERP instance in a single web service call, very soon you might notice timeout errors. Increasing timeout is a possible one-time, but not very good long-term solution. Your best option to address this challenge is to export stock items in batches of several records.

# To export stock items in batches of 10 records: #

    Screen context = new Screen();
    context.CookieContainer = new System.Net.CookieContainer();
    context.Url = "http://localhost/AcumaticaERP/Soap/IN202500.asmx";
    context.Login(username, password);
    try
    {
        Content stockItemsSchema = PX.Soap.Helper.GetSchema<Content>(context);
        Field lastModifiedField = new Field
        {
            ObjectName = stockItemsSchema.StockItemSummary.InventoryID.ObjectName,
            FieldName = "LastModifiedDateTime"
        };
        var commands = new Command[]
        {
            stockItemsSchema.StockItemSummary.ServiceCommands.EveryInventoryID,
            stockItemsSchema.StockItemSummary.InventoryID,
            stockItemsSchema.StockItemSummary.Description,
            stockItemsSchema.GeneralSettingsItemDefaults.ItemClass,
            stockItemsSchema.GeneralSettingsUnitOfMeasureBaseUnit.BaseUnit,
            lastModifiedField
        };
        var items = context.Export(commands, null, 10, false, false);

        while (items.Length == 10)
        {
            var filters = new Filter[]
            {
                new Filter
                {
                    Field = stockItemsSchema.StockItemSummary.InventoryID,
                    Condition = FilterCondition.Greater,
                    Value = items[items.Length - 1][0]
                }
            };
            items = context.Export(commands, filters, 10, false, false);
        }
    }
    finally
    {
        context.Logout();
    }

There are 2 main differences between the single call approach and the export in batches:

 - **topCount** parameter of the **Export** command was always set to `0` in the single call approach

 - when exporting records in batches, size of a batch is configured though the **topCount** parameter supplemented by the **Filter** array to request the next result set

  [1]: https://i.stack.imgur.com/4nWDU.png

## Data Export from an Entry Form with a Composite Primary Key
The **Sales Orders** screen (SO.30.10.00) is a perfect example of a data entry form with a composite primary key. The primary key on the **Sales Orders** screen is composed by the ***Order Type*** and the ***Order Number***:
[![enter image description here][1]][1]

The recommended 2-step strategy to export data from the **Sales Orders** screen or any other data entry form with a composite primary key via the Screen-Based API:

 - on step 1 you request all types of orders previously created in your Acumatica ERP application

 - 2nd step is to export orders of each type independently either in a single call or in batches

# To request all types of existing orders: #

    Screen context = new Screen();
    context.CookieContainer = new System.Net.CookieContainer();
    context.Url = "http://localhost/AcumaticaERP/Soap/SO301000.asmx";
    context.Login(username, password);
    try
    {
        Content orderSchema = PX.Soap.Helper.GetSchema<Content>(context);
        var commands = new Command[]
        {
            orderSchema.OrderSummary.ServiceCommands.EveryOrderType,
            orderSchema.OrderSummary.OrderType,
        };

        var types = context.Export(commands, null, 1, false, false);
    }
    finally
    {
        context.Logout();
    }

In the SOAP call above, notice **topCount** parameter of the **Export** command set to `1`. The purpose of this request is only to get all types of orders previously created in your Acumatica ERP application, not to export data.

# To export records of each type independently in batches: #

    Screen context = new Screen();
    context.CookieContainer = new System.Net.CookieContainer();
    context.Url = "http://localhost/AcumaticaERP/Soap/SO301000.asmx";
    context.Login(username, password);
    try
    {
        Content orderSchema = PX.Soap.Helper.GetSchema<Content>(context);
        var commands = new Command[]
        {
            orderSchema.OrderSummary.ServiceCommands.EveryOrderType,
            orderSchema.OrderSummary.OrderType,
        };
        var types = context.Export(commands, null, 1, false, false);

        for (int i = 0; i < types.Length; i++)
        {
            commands = new Command[]
            {
                new Value
                {
                    LinkedCommand = orderSchema.OrderSummary.OrderType,
                    Value = types[i][0]
                },
                orderSchema.OrderSummary.ServiceCommands.EveryOrderNbr,
                orderSchema.OrderSummary.OrderType,
                orderSchema.OrderSummary.OrderNbr,
                orderSchema.OrderSummary.Customer,
                orderSchema.OrderSummary.CustomerOrder,
                orderSchema.OrderSummary.Date,
                orderSchema.OrderSummary.OrderedQty,
                orderSchema.OrderSummary.OrderTotal
            };
            var orders = context.Export(commands, null, 100, false, false);
            while (orders.Length == 100)
            {
                var filters = new Filter[]
                {
                    new Filter
                    {
                        Field = orderSchema.OrderSummary.OrderNbr,
                        Condition = FilterCondition.Greater,
                        Value = orders[orders.Length - 1][1]
                    }
                };
                orders = context.Export(commands, filters, 100, false, false);
            }
        }
    }
    finally
    {
        context.Logout();
    }

The sample above demonstrates how to export all sales orders from Acumatica ERP in batches of 100 records. To export sales order of each type independently, your SOAP request must always begin with the `Value` command, which determines the type of orders to be exported. After the Value command used to set first key value goes the `ServiceCommands.Every[Key]` command, where `[Key]` is to be replaced with name of the second key.

# To export records of a specific type: #

In case you need to export sales orders of a specific type, it's possible to explicitly define the type of orders with the `Value` command in the beginning of your SOAP request followed by the single call approach or the export in batches.

To export all sales order of the ***IN*** type in one call:

    Screen context = new Screen();
    context.CookieContainer = new System.Net.CookieContainer();
    context.Url = "http://localhost/AcumaticaERP/Soap/SO301000.asmx";
    context.Login(username, password);
    try
    {
        Content orderSchema = PX.Soap.Helper.GetSchema<Content>(context);
        var commands = new Command[]
        {
            new Value
            {
                LinkedCommand = orderSchema.OrderSummary.OrderType,
                Value = "IN"
            },
            orderSchema.OrderSummary.ServiceCommands.EveryOrderNbr,
            orderSchema.OrderSummary.OrderType,
            orderSchema.OrderSummary.OrderNbr,
            orderSchema.OrderSummary.Customer,
            orderSchema.OrderSummary.CustomerOrder,
            orderSchema.OrderSummary.Date,
            orderSchema.OrderSummary.OrderedQty,
            orderSchema.OrderSummary.OrderTotal
        };
        var orders = context.Export(commands, null, 0, false, false);
    }
    finally
    {
        context.Logout();
    }

  [1]: https://i.stack.imgur.com/Ecn8a.png

