---
title: "Adding Attribute Support to out-of-box Sales Order Entity"
slug: "adding-attribute-support-to-out-of-box-sales-order-entity"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Acumatica ERP lets you define attributes for flexible, meaningful classification of an Entity (Lead, Stock/Non-Stock Items Etc.) as required for your company’s specific needs. An attribute is a property that enables you to specify additional information for objects in the system. Attributes are defined in the context of a class which is a grouping of the business accounts (including leads, opportunities, customers, and cases), Stock and Non-Stock items by one or more of their properties.

This example is applicable to Acumatica 6.0 series

## This article provides how-to guide to add Acumatica ERP Attribute support to out-of-box Sales Order Entity
At the very core, your entity main DAC must have GUID column (`NoteID`) to reference `CSAnswers` table and must have field that identify the class of the Entity.

We will make use of `Order Type` to define list of attributes to gather particular order type-specific information.

Create a Graph Extension for `SOOrderTypeMaint` Graph and declare data view to define list of attributes for a particular order type. We will be using out-of-box `CSAttributeGroupList<TEntityClass, TEntity>`

<!-- language: lang-cs -->
    public class SOOrderTypeMaintPXExt : PXGraphExtension<SOOrderTypeMaint>
    {
        [PXViewName(PX.Objects.CR.Messages.Attributes)]
        public CSAttributeGroupList<SOOrderType, SOOrder> Mapping;
    }

Create a Graph Extension for `SOOrderEntry` Graph and declare data view for attributes specific to current order type.

<!-- language: lang-cs -->
    public class SOOrderEntryPXExt : PXGraphExtension<SOOrderEntry>
    {
        public CRAttributeList<SOOrder> Answers;
    }

Create DAC Extension for `SOOrder` DAC and declare user defined field decorated with `CRAttributesField` attribute and specify the `ClassID` field – in our case it is `OrderType`.

<!-- language: lang-cs -->
    public class SOOrderPXExt : PXCacheExtension<SOOrder>
    {
        #region UsrAttributes

        public abstract class usrAttributes : IBqlField { }

        [CRAttributesField(typeof(SOOrder.orderType))]
        public virtual string[] UsrAttributes { get; set; }

        #endregion
    }

Modify `Order Types` page (`SO201000`) as below using Customization Engine

<!-- language: lang-html -->

    <px:PXTabItem Text="Attributes">
      <Template>
        <px:PXGrid runat="server" BorderWidth="0px" Height="150px" SkinID="Details" Width="100%" ID="AttributesGrid" 
                    MatrixMode="True" DataSourceID="ds">
            <AutoSize Enabled="True" Container="Window" MinHeight="150" />
            <Levels>
                <px:PXGridLevel DataMember="Mapping">
                    <RowTemplate>
                        <px:PXSelector runat="server" DataField="AttributeID" FilterByAllFields="True" AllowEdit="True" 
                                        CommitChanges="True" ID="edAttributeID" /></RowTemplate>
                    <Columns>
                        <px:PXGridColumn DataField="AttributeID" Width="81px" AutoCallBack="True" LinkCommand="ShowDetails" />
                        <px:PXGridColumn DataField="Description" Width="351px" AllowNull="False" />
                        <px:PXGridColumn DataField="SortOrder" TextAlign="Right" Width="81px" />
                        <px:PXGridColumn DataField="Required" Type="CheckBox" TextAlign="Center" AllowNull="False" />
                        <px:PXGridColumn DataField="CSAttribute__IsInternal" Type="CheckBox" TextAlign="Center" AllowNull="True" />
                        <px:PXGridColumn DataField="ControlType" Type="DropDownList" Width="81px" AllowNull="False" />
                        <px:PXGridColumn DataField="DefaultValue" RenderEditorText="False" Width="100px" AllowNull="True" />
                    </Columns>
                </px:PXGridLevel>
            </Levels>
        </px:PXGrid>
      </Template>
    </px:PXTabItem>

Modify `Sales Orders` page (`SO301000`) as below using Customization Engine

<!-- language: lang-html -->

    <px:PXTabItem Text="Attributes">
      <Template>
        <px:PXGrid runat="server" ID="PXGridAnswers" Height="200px" SkinID="Inquire" 
                    Width="100%" MatrixMode="True" DataSourceID="ds">
            <AutoSize Enabled="True" MinHeight="200" />
            <ActionBar>
                <Actions>
                    <Search Enabled="False" />
                </Actions>
            </ActionBar>
            <Mode AllowAddNew="False" AllowDelete="False" AllowColMoving="False" />
            <Levels>
                <px:PXGridLevel DataMember="Answers">                        
                    <Columns>
                        <px:PXGridColumn TextAlign="Left" DataField="AttributeID" TextField="AttributeID_description" 
                                            Width="250px" AllowShowHide="False" />
                        <px:PXGridColumn Type="CheckBox" TextAlign="Center" DataField="isRequired" Width="80px" />
                        <px:PXGridColumn DataField="Value" Width="300px" AllowSort="False" AllowShowHide="False" />
                    </Columns>
                </px:PXGridLevel>
            </Levels>
        </px:PXGrid>
      </Template>
    </px:PXTabItem>

[Download deployment package][1]


  [1]: https://github.com/Acumatica/PXSOAttributeSupportExtPkg

