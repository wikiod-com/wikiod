---
title: "Extending List of Entities Supported by Tasks, Events and Activities"
slug: "extending-list-of-entities-supported-by-tasks-events-and-activities"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

In this topic you will learn how to extend the Related Entity Description field  with a custom entity for Tasks, Events and Activities.

## Adding Test Work Orders to the Related Entity Description Field
Let's say you have already created the custom **Test Work Orders** screen to manage test work orders in your Acumatica ERP application:

[![enter image description here][1]][1]

There is already `NoteID` field declared in the `TestWorkOrder` DAC, managed on the **Test Work Orders** screen:

    [Serializable]
    public class TestWorkOrder : IBqlTable
    {
        ...

        #region NoteID
        public abstract class noteID : IBqlField { }
        [PXNote]
        public virtual Guid? NoteID { get; set; }
        #endregion


        ...
    }

and `ActivityIndicator` property is set to ***True*** for the top-level `PXForm` container:

    <px:PXFormView ID="form" runat="server" ActivityIndicator="true" DataSourceID="ds" Style="z-index: 100" DataMember="ITWO" Width="100%" >

However, when new task, event or activity is created for a test work order, the **Related Entity Description** control is always empty:

[![enter image description here][2]][2]

To add the **Test Work Order** entity to the **Related Entity Description** selector, you should complete the following steps:

 1. For the `PXNoteAttribute` on **TestWorkOrder.NoteID** field, set `ShowInReferenceSelector` property to ***True*** and define BQL expression to select data records displayed in the **Entity** lookup:

        [PXNote(
            ShowInReferenceSelector = true,
            Selector = typeof(Search<TestWorkOrder.orderNbr>))]
        public virtual Guid? NoteID { get; set; }


 2. Decorate the `TestWorkOrder` DAC with the `PXCacheNameAttribute` and the `PXPrimaryGraphAttribute`:

        [PXLocalizable]
        public static class Messages
        {
            public const string Opportunity = "Test Work Order";
        }

        [Serializable]
        [PXCacheName(Messages.Opportunity)]
        [PXPrimaryGraph(typeof(TestWorkOrderEntry))]
        public class TestWorkOrder : IBqlTable
        {
            ...
        }

    The `PXCacheName` attribute defines user-friendly name for the `TestWorkOrder` DAC (***Test Work Order*** in this case), which will be available in the **Type** dropdown. The `PXPrimaryGraph` attribute determines the entry page where a user is redirected for editing a test work order, which is the **Test Work Orders** screen in the given example.

 3. Decorate some `TestWorkOrder` fields with the `PXFieldDescriptionAttribute`. Those field values will be concatenated into a single text label, representing the referenced test work order inside the **Related Entity Description** field:

        ...
        [PXFieldDescription]
        public virtual string OrderNbr { get; set; }

        ...
        [PXFieldDescription]
        public virtual String Status { get; set; }

        ...
        [PXFieldDescription]
        public virtual string POOrderNbr { get; set; }

 4. Define the list of columns displayed in the **Entity** lookup by choosing one of the approaches below:

     a. Use the `PXNoteAttribute.FieldList` property (gets the highest priority):

        public abstract class noteID : IBqlField { }
        [PXNote(
            ShowInReferenceSelector = true,
            Selector = typeof(Search<TestWorkOrder.orderNbr>),
            FieldList = new Type[]
            {
                typeof(TestWorkOrder.orderNbr),
                typeof(TestWorkOrder.orderDate),
                typeof(TestWorkOrder.status),
                typeof(TestWorkOrder.poOrderNbr)
            })]
        public virtual Guid? NoteID { get; set; }

     b. Borrow the list of columns defined for the **OrderNbr** lookup:

        public abstract class orderNbr : IBqlField { }
        [PXDBString(15, IsKey = true, IsUnicode = true, InputMask = ">CCCCCCCCCCCCCCC")]
        [PXDefault()]
        [PXUIField(DisplayName = "ITWO Nbr.", Visibility = PXUIVisibility.SelectorVisible)]
        [PXSelector(typeof(Search<TestWorkOrder.orderNbr>),
            typeof(TestWorkOrder.orderNbr),
            typeof(TestWorkOrder.orderDate),
            typeof(TestWorkOrder.status),
            typeof(TestWorkOrder.poOrderNbr))]
        [PXFieldDescription]
        public virtual string OrderNbr { get; set; }

     c. Show all `TestWorkOrder` fields with **Visibility** set to `PXUIVisibility.SelectorVisible`:

        ...
        [PXUIField(DisplayName = "ITWO Nbr.", Visibility = PXUIVisibility.SelectorVisible)]
        public virtual string OrderNbr { get; set; }

        ...
        [PXUIField(DisplayName = "Order Date", Visibility = PXUIVisibility.SelectorVisible)]
        public virtual DateTime? OrderDate { get; set; }

        ...
        [PXUIField(DisplayName = "Status", Visibility = PXUIVisibility.SelectorVisible)]
        public virtual String Status { get; set; }

        ...
        [PXUIField(DisplayName = "Purchase Order", Visibility = PXUIVisibility.SelectorVisible)]
        public virtual string POOrderNbr { get; set; }

After you completed the 4 steps above, **Test Work Orders** should be fully supported by the **Related Entity Description** field on Tasks, Events and Activities

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/br7c7.png
  [2]: https://i.stack.imgur.com/QoEQC.png
  [3]: https://i.stack.imgur.com/DVy78.png

