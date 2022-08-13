---
title: "Customization Mechanisms"
slug: "customization-mechanisms"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Using CacheAttached to Override DAC Attributes in the Graph
Sometimes, you need to override one or more attributes of a particular Data Access Class (DAC) field just for a particular screen, without changing the existing behavior for other screens.

# Replacing All Attributes

Suppose the original DAC field attributes are declared as shown below:

    public class ARInvoice : IBqlTable
    {
        [PXDBDecimal(4)]
        [PXDefault(TypeCode.Decimal, "0.0")]
        [PXUIField(DisplayName = "Commission Amount")]
        public virtual Decimal? CommnAmt 
        { 
            get; 
            set; 
        }
    }

The basic way to override the field's attributes in the graph is to declare a  `CacheAttached` event handler in the graph that follows the standard convention for naming graph events (note the absence of the EventArgs argument). The body of the event handler will not be executed, but any *attributes* that you place on the handler will replace the attributes on the corresponding DAC field:

    [PXDBDecimal(4)]
    [PXDefault(TypeCode.Decimal, "0.0")]
    [PXUIField(DisplayName = "Commission Amount")]
    [PXAdditionalAttribute(NecessaryProperty = true)]
    protected virtual void ARInvoice_CommnAmt_CacheAttached(PXCache sender) { }

# Appending a New Attribute to the DAC Field

The set of attributes placed on the CacheAttached handler will **redefine the whole set of the attributes** placed on the field in the DAC. This is almost always overkill; note how in the previous example, in order to add just a single attribute to the field, you had to copy all other attribute declarations from the DAC. This leads to undesired code duplication, as well as the possibility of DAC and the graph going out of sync. It is very easy to imagine a situation when someone changes the defaulting logic of, for instance, `PXDefaultAttribute` in the DAC, but forgets to update all the corresponding attributes placed on the `CacheAttached` handlers in various graphs. 

To remedy this problem, the Acumatica Framework provides a special attribute called `PXMergeAttributesAttribute`. When this attribute is placed on a `CacheAttached` handler, you can reuse the existing attributes defined in the DAC.

Appending an attribute using `PXMergeAttributesAttribute`:

    [PXMergeAttributes(Method = MergeMethod.Append)]
    [PXAdditionalAttribute(NecessaryProperty = true)]
    protected virtual void ARInvoice_CommnAmt_CacheAttached(PXCache sender) { }

In the above example, the whole set of attributes from the original DAC will be reused, appended by any attributes that you have declared on the `CacheAttached` event handler.

`PXMergeAttributesAttribute` has other merge behaviours, according to the following possible values for the Method property:

- `MergeMethod.Replace` replaces the DAC's attributes completely (equivalent to the absence of `PXMergeAttributesAttribute`).
- `MergeMethod.Append` appends the attributes from the `CacheAttached` handler to the original DAC attributes.
- `MergeMethod.Merge` is similar to `Append`; however, it also checks whether there are any conflicting attributes between the handler attributes and the DAC field attributes. If there is a conflict, the `CacheAttached` attribute takes precedence and the corresponding DAC attribute is discarded.

# Overriding a Single Property of an Attribute

A very common application development scenario occurs when you have to redefine just a single property of a DAC's attribute for a particular screen; consider the situation when you have to define the `DisplayName` property of the PXUIFieldAttribute. 

For that purpose, you can use yet another special attribute provided by the Acumatica Framework: `PXCustomizeBaseAttributeAttribute`. Its constructor accepts three values:

- The type of the DAC attribute whose property needs to be overridden
- The name of the attribute's property to override (use the `nameof` operator in C# 6.0 for code maintainability)
- The new value for the specified property.

Suppose that there is a requirement to change the UI display name from *Commission Amount* to *Base Currency Commission* for only one screen. The following code example demonstrates how to implement the desired behavior.

    [PXMergeAttributes(Method = MergeMethod.Append)]
    [PXCustomizeBaseAttribute(typeof(PXUIFieldAttribute), nameof(PXUIFieldAttribute.DisplayName), "Base Currency Commission")]
    protected virtual void ARInvoice_CommnAmt_CacheAttached(PXCache sender) { }

In this example, `PXMergeAttributes` ensures that the original DAC attributes are preserved, and `PXCustomizeBaseAttribute` allows the software engineer to override the UI field's display name for the graph in question.

# Replacing an Attribute with Another Attribute

Suppose that there is a requirement to replace a DAC field's `PXDefaultAttribute` with `PXDBDefaultAttribute` for only one screen.

This can be achieved in the following manner:

    [PXMergeAttributes(Method = MergeMethod.Append)]
    [PXRemoveBaseAttribute(typeof(PXDefaultAttribute))]
    [PXDBDefault(typeof(SOShipment.siteID), PersistingCheck = PXPersistingCheck.Nothing)]
    protected void SOOrderShipment_SiteID_CacheAttached(PXCache sender) { }

# Application Order of the Attribute-Customizing Attributes

 1. `PXCustomizeBaseAttribute`
 2. `PXRemoveBaseAttribute`
 3. `PXMergeAttributes`

