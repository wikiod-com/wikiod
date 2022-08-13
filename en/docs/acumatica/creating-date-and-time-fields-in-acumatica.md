---
title: "Creating Date and Time Fields in Acumatica"
slug: "creating-date-and-time-fields-in-acumatica"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This topic will walk you through different options available in the Acumatica Framework to create date and time fields in a data access class (DAC).

## The PX(DB)DateAndTime Attribute
The **PXDBDateAndTime** attribute and the **PXDateAndTime** attribute are designed to work with a DAC field of the `Nullable<DateTime>` (`DateTime?`) type and store both date and time value parts inside a single field:

    #region UsrDateAndTime
    public abstract class usrDateAndTimeAttribute : IBqlField
    { }

    [PXDBDateAndTime(
        DisplayNameDate = "Date Value Part",
        DisplayNameTime = "Time Value Part")]
    public DateTime? UsrDateAndTime { get; set; }
    #endregion

From the UI perspective, for a field decorated with **PXDBDateAndTimeAttribute** or **PXDateAndTimeAttribute**, one is expected to create either separate input controls for date and time value parts:

[![enter image description here][1]][1]

    <px:PXDateTimeEdit runat="server" ID="edUsrDate" DataField="UsrDateAndTime_Date" />
    <px:PXDateTimeEdit runat="server" ID="edUsrTime" DataField="UsrDateAndTime_Time" TimeMode="True" />

or separate grid columns to enter and display date and time values:

[![enter image description here][2]][2]

    <Columns>
        ...
        <px:PXGridColumn DataField="UsrDateAndTime_Date" Width="90px" />
        <px:PXGridColumn DataField="UsrDateAndTime_Time" Width="90px" TimeMode="True" />
        ...
    </Columns>

  [1]: https://i.stack.imgur.com/G7YuA.png
  [2]: https://i.stack.imgur.com/F35eW.png

## The PXDBTime Attribute
The **PXDBTime** attribute is designed to work with a DAC field of the `Nullable<DateTime>` (`DateTime?`) type and store only the time part without date inside a DAC field:

    #region UsrTime
    public abstract class usrTime : IBqlField
    { }

    [PXDBTime(DisplayMask = "t", InputMask = "t")]
    [PXUIField(DisplayName = "Time Only Value")]
    public DateTime? UsrTime { get; set; }
    #endregion

In the UI, for a field decorated with **PXDBTimeAttribute** the system creates an input control accepting only time values both on a form:

[![enter image description here][1]][1]

    <px:PXDateTimeEdit runat="server" ID="edUsrTime" DataField="UsrTime" TimeMode="True" />

and within a grid cell:

[![enter image description here][2]][2]

    <Columns>
        ...
        <px:PXGridColumn DataField="UsrTime" Width="120px" TimeMode="True" />
        ...
    </Columns>

  [1]: https://i.stack.imgur.com/CmqAb.png
  [2]: https://i.stack.imgur.com/G3BKA.png

## The PX(DB)DateAttribute Attribute
The **PXDBDate** attribute and the **PXDate** attribute are designed to work with a DAC field of the `Nullable<DateTime>` (`DateTime?`) type and store date value with an optional time part inside a single field. Wheather **PX(DB)DateAttribute** should save time in addition to date in a DAC field is defined by the **PreserveTime** property: when **PreserveTime** is set to ***True***, the time part of a field value is preserved, otherwise only the date part is saved in a DAC field:

    #region UsrDateTime
    public abstract class usrDateTime : IBqlField
    { }

    [PXDBDate(PreserveTime = true, InputMask = "g")]
    [PXUIField(DisplayName = "DateTime Value")]
    public DateTime? UsrDateTime { get; set; }
    #endregion

    #region UsrDate
    public abstract class usrDate : IBqlField
    { }

    [PXDBDate]
    [PXUIField(DisplayName = "Date Value")]
    public DateTime? UsrDate { get; set; }
    #endregion

In the UI, for a field decorated with **PXDBDateAttribute** or **PXDateAttribute** the system creates an input control accepting either only date values or both date and time values depending on the value of **PreserveTime** property. This concept works exactly the same on a form:

[![enter image description here][1]][1]

    <px:PXDateTimeEdit runat="server" ID="edUsrDateTime" DataField="UsrDateTime" Size="SM" />
    <px:PXDateTimeEdit runat="server" ID="edUsrDate" DataField="UsrDate" />

and within a grid cell:

[![enter image description here][2]][2]

    <Columns>
        ...
        <px:PXGridColumn DataField="UsrDateTime" Width="130px" />
        <px:PXGridColumn DataField="UsrDate" Width="90px" />
        ...
    </Columns>

  [1]: https://i.stack.imgur.com/fVzhH.png
  [2]: https://i.stack.imgur.com/LrAdV.png

## The PXDBTimeSpan Attribute
The **PXDBTimeSpan** attribute is designed to work with a DAC field of the `Nullable<int>` (`int?`) type and store time value inside a DAC field as the number of minutes passed since midnight:

    #region UsrTimeInt
    public abstract class usrTimeInt : IBqlField
    { }

    [PXDBTimeSpan(DisplayMask = "t", InputMask = "t")]
    [PXUIField(DisplayName = "Time Value")]
    public int? UsrTimeInt { get; set; }
    #endregion

In the UI, for a field decorated with **PXDBTimeSpanAttribute** the system creates a drop-down with half hour interval values both on a form:

[![enter image description here][1]][1]

and within a grid cell:

    <px:PXDateTimeEdit runat="server" ID="edUsrTimeInt" DataField="UsrTimeInt" TimeMode="true" />


[![enter image description here][2]][2]


    <px:PXGridColumn DataField="UsrTimeInt" Width="90px" TimeMode="true" />


  [1]: https://i.stack.imgur.com/vOgIa.png
  [2]: https://i.stack.imgur.com/OUp3x.png

## The PXTimeList Attribute
The **PXTimeList** attribute is designed to work with a DAC field of the `Nullable<int>` (`int?`) type and store time span value inside a DAC field as a number of minutes:

    #region UsrTimeSpan
    public abstract class usrTimeSpan : IBqlField
    { }

    [PXDBInt]
    [PXTimeList]
    [PXUIField(DisplayName = "Time Span")]
    public int? UsrTimeSpan { get; set; }
    #endregion

In the UI, for a field decorated with **PXTimeListAttribute** the system creates a drop-down with 30-minute interval values both on a form:

[![enter image description here][1]][1]

    <px:PXTimeSpan ID="edUsrTimeSpan" runat="server" DataField="UsrTimeSpan" InputMask="hh:mm" />

and within a grid cell:

[![enter image description here][2]][2]

    <RowTemplate>
        ...
        <px:PXTimeSpan ID="edgUsrTimeSpan" runat="server" DataField="UsrTimeSpan" InputMask="hh:mm" />
        ...
    </RowTemplate>
    <Columns>
    ...
        <px:PXGridColumn DataField="UsrTimeSpan" Width="90px" RenderEditorText="True" />
    ...
    </Columns>


  [1]: https://i.stack.imgur.com/0ZhYh.png
  [2]: https://i.stack.imgur.com/oKEnE.png

