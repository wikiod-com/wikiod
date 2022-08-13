---
title: "Acumatica Platform Attributes Reference"
slug: "acumatica-platform-attributes-reference"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## PXFormula Attribute
# General Description

A formula in Acumatica is a DAC field that is calculated based on the values of other object fields. 

To calculate a formula, Aсumatiсa framework provides a set of various operations and functions (such as arithmetical, logical, and comparison operations and string processing functions; see *List of Built-In Common Formulas*). In addition to the field values, a formula can use various constants provided by both the core of Acumatica and the application solutions. Moreover, a formula can obtain values for the calculation not only from the current record but also from other sources (see *Formula Context and Its Modifiers*).

The beauty of the formulas is that they will automatically recalculate the value at the right time:

* On field defaulting (inserting a new row; `FieldDefaulting` event handler of formula field)
* On updating of dependent fields (`FieldUpdated` event handler of each dependent field)
* On database selection (only for unbound fields; `RowSelecting` event handler)
* On database persisting if needed (developer should specify it explicitly; `RowPersisted` event handler)

Recalculation of a formula field value on the update of a dependent field raises a `FieldUpdated` event for formula field. This allows you to make a chain of dependent formulas (see Direct and Mediated Circular References in Formulas).

Application developers can write their own application-side formulas.

# Modes of Usage

A formula can be used in three main modes:

* Simply calculating the value and assigning it to formula field (see *Basic Usage*)
* Calculating the aggregate value from existing values of formula fields and assigning it to specified field in the parent object (see *Aggregate Usage*)
* Mixed mode: Calculating the formula value, assigning it to the formula field, calculating the aggregate value, and assigning it to the field in the parent object (see *Combined Usage*)

There is another auxiliary mode, unbound formulas, that is very similar to mixed mode, but the calculated values of the formula are not assigned to the formula field. The aggregated value is calculated immediately and assigned to the field of the parent object. See *Usage of Unbound Formulas* for more information.

# `PXFormulaAttribute` Properties and Constructor Parameters

The formula functionality is implemented by `PXFormulaAttribute`. The constructor of PXFormulaAttribute has the following signatures:

```c#
public PXFormulaAttribute(Type formulaType)
{
    // ...
}
```   

The single parameter `formulaType` is a type of formula expression to calculate the field value from other fields of the same data record. This parameter must meet one of the following conditions:

* Must implement the IBqlField interface
* Must be a BQL constant
* Must implement the IBqlCreator interface (see *List of Built-In Common Formulas*)

```c#
public PXFormulaAttribute(Type formulaType, Type aggregateType)
{
    // ...
}
```

The first parameter, `formulaType`, is the same as in the first constructor. The second parameter, `aggregateType`, is a type of aggregation formula to calculate the parent data record field from the child data record fields. An aggregation function can be used, such as SumCalc, CountCalc, MinCalc, and MaxCalc. Application developers can create their own aggregation formulas.

An aggregate formula type must be a generic type and must implement `IBqlAggregateCalculator` interface. The first generic parameter of the aggregate formula type must implement the `IBqlField` interface and must have the field type of the parent object.

```c#
public virtual bool Persistent { get; set; }
```

The `PXFormulaAttribute.Persistent` property indicates whether the attribute recalculates the formula after changes are saved to the database. You may need recalculation if the fields the formula depends on are updated on the `RowPersisting` event. By default, the property equals `false`.

# Usage

In most cases, formulas are used for direct calculation of the value of the formula field from other fields of the same data record.

The simplest example of formula usage:

```c#
[PXDBDate]
[PXFormula(typeof(FADetails.receiptDate))]
[PXDefault]
[PXUIField(DisplayName = Messages.PlacedInServiceDate)]
public virtual DateTime? DepreciateFromDate { get; set; }
```

In this example, the value of the ReceiptDate field is assigned to the DepreciateFromDate field on the insertion of a new record and on the update of the ReceiptDate field.

A slightly more complex example:

```c#
[PXCurrency(typeof(APPayment.curyInfoID), typeof(APPayment.unappliedBal))]
[PXUIField(DisplayName = "Unapplied Balance", Visibility = PXUIVisibility.Visible, Enabled = false)]
[PXFormula(typeof(Sub<APPayment.curyDocBal, APPayment.curyApplAmt>))]
public virtual Decimal? CuryUnappliedBal { get; set; }
```

Here, the unapplied balance of the document is calculated as the difference between the balance of the document and the applied amount.

Example of multiple choice with a default value:

```c#
[PXUIField(DisplayName = "Class Icon", IsReadOnly = true)]
[PXImage]
[PXFormula(typeof(Switch<
    Case<Where<EPActivity.classID, Equal<CRActivityClass.task>>, EPActivity.classIcon.task,
    Case<Where<EPActivity.classID, Equal<CRActivityClass.events>>, EPActivity.classIcon.events,
    Case<Where<EPActivity.classID, Equal<CRActivityClass.email>,
        And<EPActivity.isIncome, NotEqual<True>>>, EPActivity.classIcon.email,
    Case<Where<EPActivity.classID, Equal<CRActivityClass.email>,
        And<EPActivity.isIncome, Equal<True>>>, EPActivity.classIcon.emailResponse,
    Case<Where<EPActivity.classID, Equal<CRActivityClass.history>>, EPActivity.classIcon.history>>>>>,
    Selector<Current2<EPActivity.type>, EPActivityType.imageUrl>>))]
public virtual string ClassIcon { get; set; }
```

# Order of Fields

The order of fields in the DAC is important to correct formula calculation. All source fields (from which the formula is calculated) including other formulas must be defined in the DAC before the formula field. Otherwise, the field can be calculated incorrectly or can cause a runtime error.

# Formula Context and Its Modifiers

By default, the context of the formula calculation is restricted by the current object (record) of the class containing the formula declaration. It is also allowed to use constants (descendants of the `Constant<>` class).

A formula that uses the fields of its object only:

```c#
public partial class Contract : IBqlTable, IAttributeSupport
{
    //...
    [PXDecimal(4)]
    [PXDefault(TypeCode.Decimal, "0.0", PersistingCheck = PXPersistingCheck.Nothing)]
    [PXFormula(typeof(Add<Contract.pendingRecurring, Add<Contract.pendingRenewal, Contract.pendingSetup>>))]
    [PXUIField(DisplayName = "Total Pending", Enabled=false)]
    public virtual decimal? TotalPending { get; set; }
    //...
}
```

However, it is possible to obtain input values for the formula calculation from other sources:

* A current record of any cache in the BLC (if assigned).
* A foreign record specified by `PXSelectorAttribute`.
* A parent record specified by `PXParentAttribute`.

The formula supports the following context modifiers.

### `Current<TRecord.field>` and `Current2<TRecord.field>`

Fetches the field value from the record stored in the `Current` property of the TRecord cache.

If the cache's `Current` property __or the field itself__ contains null:

* Current<> forces field defaulting and returns the default field value.
* Current2<> returns null. 

Example:

```c#
[PXFormula(typeof(Switch<
    Case<Where<
        ARAdjust.adjgDocType, Equal<Current<ARPayment.docType>>,
        And<ARAdjust.adjgRefNbr, Equal<Current<ARPayment.refNbr>>>>,
        ARAdjust.classIcon.outgoing>,
    ARAdjust.classIcon.incoming>))]
protected virtual void ARAdjust_ClassIcon_CacheAttached(PXCache sender)
```

### `Parent<TParent.field>`

Fetches the field value from the parent data record as defined by PXParentAttribute residing on the current DAC.

```c#
public class INTran : IBqlTable
{
    [PXParent(typeof(Select<
        INRegister, 
        Where<
            INRegister.docType, Equal<Current<INTran.docType>>,
            And<INRegister.refNbr,Equal<Current<INTran.refNbr>>>>>))]
    public virtual String RefNbr { ... }
  
    [PXFormula(typeof(Parent<INRegister.origModule>))]
    public virtual String OrigModule { ... }
}
```

### `IsTableEmpty<TRecord>`

Returns `true` if the DB table corresponding to the specified DAC contains no records, `false` otherwise.

```c#
public class APRegister : IBqlTable
{
    [PXFormula(typeof(Switch<
        Case<Where<
            IsTableEmpty<APSetupApproval>, Equal<True>>,
            True,
        Case<Where<
            APRegister.requestApproval, Equal<True>>,
            False>>,
        True>))]
    public virtual bool? DontApprove { get; set; }
}
```

### `Selector<KeyField, ForeignOperand>`

# Fetches a PXSelectorAttribute defined on the foreign key field (KeyField) of the current DAC.
# Fetches the foreign data record currently referenced by the selector.
# Calculates and returns an expression on that data record as defined by ForeignOperand.

```c#
public class APVendorPrice : IBqlTable
{
    // Note: inventory attribute is an
    // aggregate containing a PXSelectorAttribute
    // inside, which is also valid for Selector<>.
    // -
    [Inventory(DisplayName = "Inventory ID")]
    public virtual int? InventoryID
  
    [PXFormula(typeof(Selector<
        APVendorPrice.inventoryID, 
        InventoryItem.purchaseUnit>))]
    public virtual string UOM { get; set; }
}
```

# Using Formulas on Unbound Fields

If the formula field is an unbound field marked with one of the `PXFieldAttribute` descendants (such as `PXIntAttribute` or `PXStringAttribute`), then its calculation is additionally triggered during `RowSelecting` event.

# List of Built-In Common Formulas

TBD

# Direct and Mediated Circular References in Formulas

TBD

# Control Flow in Conditional Formulas

TBD

# Using Multiple Formulas on One Field

TBD

## PXRestrictor Attribute
# Introduction

The PXSelectorAttribute attribute (also referred to as the selector), while vital and frequently used, has however two major drawbacks:

* It gives an uninformative message `"<object_name> cannot be found in the system"` if no items are found to satisfy the selector condition.
* The produces the same error message if you update *other* fields of the record but the object referenced by the selector has already changed and no longer meets its condition. This behaviour is clearly wrong because the law must not be retroactive.

The `PXRestrictorAttribute` (also referred to as the restrictor) can be used to solve these problems.

# Details

`PXRestrictorAttribute` does not work alone; it should always be paired with a `PXSelectorAttribute`. Using the restrictor without the selector will have no effect.

The restrictor finds the selector on the same field, injecting into it an additional condition and the corresponding error message. The restrictor condition is appended to the selector condition via a boolean AND, and an appropriate error message is generated if the referenced object violates the restrictor constraint. Also, if the referenced object has changed and no longer meets the restrictor condition, no error messages are produced when you change **any other** fields of the referencing object.

General usage:

```c#
[PXDBInt]
[PXSelector(typeof(Search<FAClass.assetID, Where<FAClass.recordType, Equal<FARecordType.classType>>>),
    typeof(FAClass.assetCD), typeof(FAClass.assetTypeID), typeof(FAClass.description), typeof(FAClass.usefulLife),
    SubstituteKey = typeof(FAClass.assetCD),
    DescriptionField = typeof(FAClass.description), CacheGlobal = true)]
[PXRestrictor(typeof(Where<FAClass.active, Equal<True>>), Messages.InactiveFAClass, typeof(FAClass.assetCD))]
[PXUIField(DisplayName = "Asset Class", Visibility = PXUIVisibility.Visible)]
public virtual int? ClassID { get; set; }
```

Multiple restrictors can be used with one selector attribute. In this case, all additional restrictor conditions are applied in a non-determined order. Once any condition is violated, the appropriate error message is generated.

The `Where<>` condition of the selector itself is applied **after** all restrictor conditions.

```c#
[PXDefault]
//  An aggregate attribute containing the selector inside.
// -
[ContractTemplate(Required = true)]
[PXRestrictor(typeof(Where<ContractTemplate.status, Equal<Contract.status.active>>), Messages.TemplateIsNotActivated, typeof(ContractTemplate.contractCD))]
[PXRestrictor(typeof(Where<ContractTemplate.effectiveFrom, LessEqual<Current<AccessInfo.businessDate>>, 
    Or<ContractTemplate.effectiveFrom, IsNull>>), Messages.TemplateIsNotStarted)]
[PXRestrictor(typeof(Where<ContractTemplate.discontinueAfter, GreaterEqual<Current<AccessInfo.businessDate>>, 
    Or<ContractTemplate.discontinueAfter, IsNull>>), Messages.TemplateIsExpired)]
public virtual int? TemplateID { get; set; }
```

# Options

The constructor of PXRestrictorAttribute takes three parameters:

1. The restrictor's additional condition. This BQL type must implement the `IBqlWhere` interface.
2. The appropriate error message. The message can contain format elements (curly brackets) to show context. The message must be a string constant defined in a localizable static class (such as `PX.Objects.GL.Messages`).
3. An array of field types. These fields must belong to the current object and must implement the `IBqlField` interface. The values of the fields will be used for error message formatting.

Also, there are several options that specify the restrictor behavior.

## Overriding Inherited Restrictors

The `ReplaceInherited` property indicates whether the current restrictor should override the inherited restrictors. If this property is set to true, then all inherited restrictors (placed on any aggregate attributes or base attribute) will be replaced.

Replacing inherited restrictors:

```c#
 [CustomerActive(Visibility = PXUIVisibility.SelectorVisible, Filterable = true, TabOrder = 2)]
 [PXRestrictor(typeof(Where<Customer.status, Equal<CR.BAccount.status.active>,
    Or<Customer.status, Equal<CR.BAccount.status.oneTime>,
    Or<Customer.status, Equal<CR.BAccount.status.hold>,
    Or<Customer.status, Equal<CR.BAccount.status.creditHold>>>>>), Messages.CustomerIsInStatus, typeof(Customer.status), 
    ReplaceInherited = true)] // Replaced all restrictors from CustomerActiveAttribute
[PXUIField(DisplayName = "Customer")]
[PXDefault()]
public override int? CustomerID { get; set; }
```

Please note that we do not advise that you use the `ReplaceInherited` property in application code when reasonable alternatives exist. This property is primarily intended to be used in customizations.

## Global Caching

`CacheGlobal` supports global dictionary functionality in the same way as in `PXSelectorAttribute`. 

# Recommendations for Using
## Use Restrictor Conditions Only

When restrictors and a selector are used together, the latter should not contain the `IBqlWhere` clause. Ideally, all conditions should be moved into restrictors. This approach provides more user-friendly error messages and eliminates unnecessary retroactive errors.

An ideal example:

```c#
[PXDBString(5, IsFixed = true, IsUnicode = false)]
[PXUIField(DisplayName = "Type", Required = true)]
[PXSelector(typeof(EPActivityType.type), DescriptionField = typeof(EPActivityType.description))]
[PXRestrictor(typeof(Where<EPActivityType.active, Equal<True>>), Messages.InactiveActivityType, typeof(EPActivityType.type))]
[PXRestrictor(typeof(Where<EPActivityType.isInternal, Equal<True>>), Messages.ExternalActivityType, typeof(EPActivityType.type))]
public virtual string Type { get; set; }
```

Possible retroactive errors:

```c#
[PXDBInt]
[PXUIField(DisplayName = "Contract")]
[PXSelector(typeof(Search2<Contract.contractID,
    LeftJoin<ContractBillingSchedule, On<Contract.contractID, Equal<ContractBillingSchedule.contractID>>>,
    Where<Contract.isTemplate, NotEqual<True>,
        And<Contract.baseType, Equal<Contract.ContractBaseType>,
        And<Where<Current<CRCase.customerID>, IsNull,
            Or2<Where<Contract.customerID, Equal<Current<CRCase.customerID>>,
                And<Current<CRCase.locationID>, IsNull>>,
            Or2<Where<ContractBillingSchedule.accountID, Equal<Current<CRCase.customerID>>,
                And<Current<CRCase.locationID>, IsNull>>,
            Or2<Where<Contract.customerID, Equal<Current<CRCase.customerID>>,
                And<Contract.locationID, Equal<Current<CRCase.locationID>>>>,
            Or<Where<ContractBillingSchedule.accountID, Equal<Current<CRCase.customerID>>,
                And<ContractBillingSchedule.locationID, Equal<Current<CRCase.locationID>>>>>>>>>>>>,
    OrderBy<Desc<Contract.contractCD>>>),
    DescriptionField = typeof(Contract.description),
    SubstituteKey = typeof(Contract.contractCD), Filterable = true)]
[PXRestrictor(typeof(Where<Contract.status, Equal<Contract.status.active>>), Messages.ContractIsNotActive)]
[PXRestrictor(typeof(Where<Current<AccessInfo.businessDate>, LessEqual<Contract.graceDate>, Or<Contract.expireDate, IsNull>>), Messages.ContractExpired)]
[PXRestrictor(typeof(Where<Current<AccessInfo.businessDate>, GreaterEqual<Contract.startDate>>), Messages.ContractActivationDateInFuture, typeof(Contract.startDate))]       
[PXFormula(typeof(Default<CRCase.customerID>))]
[PXDefault(PersistingCheck = PXPersistingCheck.Nothing)]
public virtual int? ContractID { get; set; }
```

