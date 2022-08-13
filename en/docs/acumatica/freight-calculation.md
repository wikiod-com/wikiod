---
title: "Freight Calculation"
slug: "freight-calculation"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Acumatica ERP enables you to manage freight to better control any additional costs and revenues on sales transactions. The freight amount you charge your customers may include not only the freight your company is charged by carriers, but also insurance, handling and packaging fees defined by your shipping terms and premium freight.

## Overriding Freight Amount in Shipment and Invoice
Out of the box Acumatica allows to create and maintain the list of shipping terms in the system. Shipping terms are used to define the shipping, packaging and handling costs, depending on the shipment amount. 

In this example I will show how to calculate freight amount for a shipment based on sales order amount, which would allow users to create multiple shipments per sales order with same shipping terms automatically applied to all shipments.

# FreightCalculator #

The `FreightCalculator` class is responsible for calculation of Freight Cost and Freight Terms. For the purpose of this example, our interest will be only around the `GetFreightTerms` method:

    public class FreightCalculator
    {
        ...

        protected virtual ShipTermsDetail GetFreightTerms(string shipTermsID, decimal? lineTotal)
        {
            return PXSelect<ShipTermsDetail,
                Where<ShipTermsDetail.shipTermsID, Equal<Required<SOOrder.shipTermsID>>,
                And<ShipTermsDetail.breakAmount, LessEqual<Required<SOOrder.lineTotal>>>>,
                OrderBy<Desc<ShipTermsDetail.breakAmount>>>.Select(graph, shipTermsID, lineTotal);
        }

        ...
    }

Both the **Sales Orders** and the **Shipments** screens utilize `FreightCalculator` class to calculate freight amount based on sales order's and shipment's amount respectively:

## Sales Orders ##

    public class SOOrderEntry : PXGraph<SOOrderEntry, SOOrder>, PXImportAttribute.IPXPrepareItems
    {
        ...

        public virtual FreightCalculator CreateFreightCalculator()
        {
            return new FreightCalculator(this);
        }

        ...

        protected virtual void SOOrder_RowUpdated(PXCache sender, PXRowUpdatedEventArgs e)
        {
            ...

            PXResultset<SOLine> res = Transactions.Select();
            FreightCalculator fc = CreateFreightCalculator();
            fc.CalcFreight<SOOrder, SOOrder.curyFreightCost, SOOrder.curyFreightAmt>(sender, (SOOrder)e.Row, res.Count);

            ...
        }

        ...
    }

## Shipments ##

    public class SOShipmentEntry : PXGraph<SOShipmentEntry, SOShipment>
    {
        ...

        protected virtual FreightCalculator CreateFreightCalculator()
        {
            return new FreightCalculator(this);
        }

        ...

        protected virtual void SOShipment_RowUpdated(PXCache sender, PXRowUpdatedEventArgs e)
        {
            ...

            PXResultset<SOShipLine> res = Transactions.Select();
            ...
            FreightCalculator fc = CreateFreightCalculator();
            fc.CalcFreight<SOShipment, SOShipment.curyFreightCost, SOShipment.curyFreightAmt>(sender, (SOShipment)e.Row, res.Count);

            ...
        }

        ...
    }

# Overriding Freight Amount #

To customize how Acumatica calculates freight amount on the **Shipments** screen I will declare `FreightCalculatorCst` class inherited from `FreightCalculator` and override `GetFreightTerms` method:

    public class FreightCalculatorCst : FreightCalculator
    {
        public FreightCalculatorCst(PXGraph graph)
            : base(graph)
        {
        }

        protected override ShipTermsDetail GetFreightTerms(string shipTermsID, decimal? lineTotal)
        {
            if (graph is SOShipmentEntry)
            {
                var shipmentEntry = graph as SOShipmentEntry;
                int orderCount = 0;
                decimal? lineTotalTemp = null;

                foreach (PXResult<SOOrderShipment, SOOrder, CurrencyInfo, SOAddress, SOContact, SOOrderType> orderRec in 
                    shipmentEntry.OrderList.SelectWindowed(0, 2))
                {
                    orderCount++;
                    SOOrder order = (SOOrder)orderRec;
                    if (orderCount == 1)
                        lineTotalTemp = order.LineTotal;
                    else
                        break;
                }

                if (orderCount == 1)
                {
                    lineTotal = lineTotalTemp;
                }
            }

            return base.GetFreightTerms(shipTermsID, lineTotal);
        }
    }

After that I will implement an extension for the `SOShipmentEntry` BLC and override `CreateFreightCalculator` method to replace `FreightCalculator` with my custom `FreightCalculatorCst` class on the **Shipments** screen:

    public class SOShipmentEntryExt : PXGraphExtension<SOShipmentEntry>
    {
        [PXOverride]
        public FreightCalculator CreateFreightCalculator()
        {
            return new FreightCalculatorCst(Base);
        }
    }

# Understanding implementation of the FreightCalculatorCst class in the sample above

In the overridden `GetFreightTerms` method I will use amount from sales order instead of shipment amount to invoke base `GetFreightTerms` method and receive shipping terms:

    foreach (PXResult<SOOrderShipment, SOOrder, CurrencyInfo, SOAddress, SOContact, SOOrderType> orderRec in 
        shipmentEntry.OrderList.SelectWindowed(0, 2))
    {
        orderCount++;
        SOOrder order = (SOOrder)orderRec;
        if (orderCount == 1)
            lineTotalTemp = order.LineTotal;
        else
            break;
    }

    if (orderCount == 1)
    {
        lineTotal = lineTotalTemp;
    }

Obviously, it's only possible to use sales order amount to calculate freight amount for shipments, which fulfill only 1 order. If one shipment fulfills several orders, we'd have to follow base product behavior and calculate freight amount based on shipment amount. To check the number of orders shipment fulfills, I used `SelectWindowed` method on the `OrderList` data view and requested first 2 orders fulfilled by the current shipment. I could have requested all orders fulfilled by the shipment, but this would take significantly more time to execute and return way to many records than needed to verify if  sales order amount can be used instead of shipment amount to calculate freight.

