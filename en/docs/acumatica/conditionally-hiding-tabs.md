---
title: "Conditionally Hiding Tabs"
slug: "conditionally-hiding-tabs"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

In this topic you will explore two approaches to conditionally hiding tabs on data entry screens in Acumatica.

## AllowSelect Property on Data Views
Unlike the **VisibleExp** property, defined in Aspx, you manipulate **AllowSelect** property of a data view though BLC or BLC extension code. The **AllowSelect** property makes it possible to use more complex boolean expressions (in comparison to the **VisibleExp** property) and, if necessary, retrieve additional information from database or other sources not available on a web page.

Below are 3 most common scenarios to work with the **AllowSelect** property:

 - **RowSelected** event handler for top-level entity to hide **Applications** tab for invoices of ***Cash Sale*** and ***Cash Return*** types:

        public class SOInvoiceEntry : ARInvoiceEntry
        {
            ...
            protected override void ARInvoice_RowSelected(PXCache cache, PXRowSelectedEventArgs e)
            {
                ...

                Adjustments.AllowSelect =
                    doc.DocType != ARDocType.CashSale &&
                    doc.DocType != ARDocType.CashReturn;
            }
            ...
        }

 - BLC constructor to show **Subitem Replenishment Info** tab on the **Item warehouse Details** screen only when both *Inventory Replenishment* and *Inventory Subitems* features are activated:

        public class INItemSiteMaint : PXGraph<INItemSiteMaint, INItemSite>
        {
            ...
            public INItemSiteMaint()
            {
                ...        

                bool enableSubItemReplenishment = PXAccess.FeatureInstalled<FeaturesSet.replenishment>() && PXAccess.FeatureInstalled<FeaturesSet.subItem>();
                subitemrecords.AllowSelect = enableSubItemReplenishment;
            }
            ...
        }

 - **RowSelected** handler for top-level entity to hide **Depreciation History** tab unless  current asset is depreciable and **Depreciation History View** is set to ***Side by Side*** in the Fixed Assets Preferences:

        public class AssetMaint : PXGraph<AssetMaint, FixedAsset>
        {
            ...
            protected virtual void FixedAsset_RowSelected(PXCache sender, PXRowSelectedEventArgs e)
            {
                ...

                AssetHistory.AllowSelect = asset.Depreciable == true && fasetup.Current.DeprHistoryView == FASetup.deprHistoryView.SideBySide;
            }
            ...
        }

Every time **AllowSelect** property is used to conditionally change tab visibility though BLC or BLC extension code, you must set **RepaintOnDemand** property to ***false*** in Aspx for the corresponding PXTab container:

    <px:PXTabItem Text="Depreciation History" RepaintOnDemand="false">

The **RepaintOnDemand** property is ***true*** by default. This property controls the initialization of PXTab container: when set to ***true***, PXTab will not be initialized until it was selected by a user. Obviously you need **RepaintOnDemand** set to ***false*** to guarantee proper behavior of the given PXTab container despite whether it was selected or not.

# To hide Cross-Reference tab for Stock Items that can not be sold #

To hide **Cross-Reference** tab from the **Stock Items** screen (IN.20.25.00) for items with ***No Sales*** status, proceed as follows:

 1. implement **InventoryItem_RowSelected** handler in the InventoryItemMaint BLC extension to set **AllowSelect** property to ***false*** for the `itemxrefrecords` data view if **Item Status** was set to ***No Sales***:

        public class InventoryItemMaintExt : PXGraphExtension<InventoryItemMaint>
        {
            protected void InventoryItem_RowSelected(PXCache sender, PXRowSelectedEventArgs e)
            {
                InventoryItem item = (InventoryItem)e.Row;
                if (item == null) return;

                Base.itemxrefrecords.AllowSelect = (item.ItemStatus != InventoryItemStatus.NoSales);
            }
        }

 2. in Customization manager, set **RepaintOnDemand** property to ***false*** for the **Cross-Reference** tab and publish customization:
    [![enter image description here][1]][1]

After you completed 2 quite simple steps above, the **Cross-Reference** tab should not be accessible for Stock Items with ***No Sales*** status:

[![enter image description here][2]][2]

# To hide Attributes tab for inactive Stock Items #

To conditionally hide ** Attributes** tab from the **Stock Items** screen (IN.20.25.00), proceed as follows:

 1. implement **InventoryItem_RowSelected** handler in the InventoryItemMaint BLC extension to set **AllowSelect** property to ***false*** for the `Answers` and `Category` data views if **Item Status** was set to ***Inactive***. Also notice **Visible** property set to ***false*** for `PXUIFieldAttribute` added on the `InventoryItem.ImageUrl` field by **CacheAttached** handler:

        public class InventoryItemMaintExt : PXGraphExtension<InventoryItemMaint>
        {
            protected void InventoryItem_RowSelected(PXCache sender, PXRowSelectedEventArgs e)
            {
                InventoryItem item = (InventoryItem)e.Row;
                if (item == null) return;

                bool showAttributesTab = item.ItemStatus != InventoryItemStatus.Inactive;
                Base.Answers.AllowSelect = Base.Category.AllowSelect = showAttributesTab;
                PXUIFieldAttribute.SetVisible<InventoryItem.imageUrl>(sender, item, showAttributesTab);
            }

            [PXMergeAttributes(Method = MergeMethod.Append)]
            [PXUIField(DisplayName = "Image")]
            protected void InventoryItem_ImageURL_CacheAttached(PXCache sender)
            { }
        }

 2. in Customization manager, set **RepaintOnDemand** property to ***false*** for the **Attributes** tab and publish customization:
    [![enter image description here][3]][3]

After you completed 2 steps above, the **Attributes** tab should not be accessible for Stock Items with ***Inactive*** status:

[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/d5YjC.png
  [2]: https://i.stack.imgur.com/M5wdG.png
  [3]: https://i.stack.imgur.com/QAhmT.png
  [4]: https://i.stack.imgur.com/hY7v0.png

## VisibleExp Property of the PXTab Control in Aspx
The **VisibleExp** property is a boolean expression, that determines if given tab is visible (when logical expression is TRUE) or hidden. You specify **VisibleExp** property for PXTab controls in Aspx page:

    <px:PXTabItem Text="Credit Card Processing Info" BindingContext="form" 
        VisibleExp="DataControls[&quot;chkIsCCPayment&quot;].Value = 1">

**VisibleExp** is composed of input controls placed within the container with ID specified in the **BindingContext** property of PXTab control. You are not allowed to use input controls from more than one container. Access to a specific input control is provided through the `DataControls` dictionary by its ID, not the name of a DAC field.

Usually **VisibleExp** property is used to compose fairly simple boolean expressions with hardcoded input control values, that are unlikely to change with time. For instance, the following expression is used on the **Sales Orders** screen (SO.30.10.00) to hide **Payment Setting** tab for orders of the ***Transfer*** type:

    <px:PXTabItem Text="Payment Settings" 
        VisibleExp="DataControls[&quot;edOrderType&quot;].Value!=TR" BindingContext="form">


# To hide Activities tab for Leads with New status #

To hide **Activities** tab from the **Leads** screen (CR.30.10.00), set ***BindingContext*** property to ***form*** (top-level *Lead Summary* form holds ***form*** ID) and define **VisibleExp** to return FALSE if lead status is Open (*Status* dropdown holds ***edStatus*** ID):

    <px:PXTabItem Text="Activities" LoadOnDemand="True" 
        BindingContext="form" VisibleExp="DataControls[&quot;edStatus&quot;].Value != H">

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/qlAkg.png

