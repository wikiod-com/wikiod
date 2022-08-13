---
title: "Filtering with multiple value with only one selector"
slug: "filtering-with-multiple-value-with-only-one-selector"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Here is a way of having multiple value inside of a selector in order to filter a grid.

## Retrieving Sales Order for multilple customer
When trying to filter some record using multiple value in a selector.
First you must use the px:PXMultiSelector in the aspx page instead of the normal px:PXSelector. Then after you must create yourself a graph containing at least three views and a view delegate. you will also need at least a basic unbound DAC.

Here is an sample page with the px:PXMultiSelector:

    <%@ Page Language="C#" MasterPageFile="~/MasterPages/FormDetail.master" AutoEventWireup="true" ValidateRequest="false" CodeFile="TT000000.aspx.cs" Inherits="Page_TT000000" Title="Untitled Page" %>

    <%@ MasterType VirtualPath="~/MasterPages/FormDetail.master" %>

    <asp:Content ID="cont1" ContentPlaceHolderID="phDS" runat="Server">
    <px:PXDataSource ID="ds" runat="server" Visible="True" Width="100%"
        TypeName="MultiSelector.MultiInquiry"
        PrimaryView="MasterView">
        <CallbackCommands>
        </CallbackCommands>
    </px:PXDataSource>
    </asp:Content>
    <asp:Content ID="cont2" ContentPlaceHolderID="phF" runat="Server">
    <px:PXFormView ID="form" runat="server" DataSourceID="ds" DataMember="MasterView" Width="100%" Height="100px" AllowAutoHide="false">
        <Template>
            <px:PXMultiSelector ID="edInventoryID" runat="server" Width="100%" DataSourceID="ds" DataField="Customer" CommitChanges="True"></px:PXMultiSelector>
        </Template>
    </px:PXFormView>
    </asp:Content>
    <asp:Content ID="cont3" ContentPlaceHolderID="phG" runat="Server">
    <px:PXGrid ID="grid" runat="server" DataSourceID="ds" Width="100%" Height="150px" SkinID="Details" AllowAutoHide="false">
        <Levels>
            <px:PXGridLevel DataMember="DetailsView">
                <Columns>
                    <px:PXGridColumn DataField="OrderType" Width="70"></px:PXGridColumn>
                    <px:PXGridColumn DataField="OrderNbr" Width="200"></px:PXGridColumn>
                    <px:PXGridColumn DataField="OrderDesc" Width="100"></px:PXGridColumn>
                    <px:PXGridColumn DataField="CustomerOrderNbr" Width="100"></px:PXGridColumn>
                    <px:PXGridColumn DataField="Status" Width="100"></px:PXGridColumn>
                    <px:PXGridColumn DataField="RequestDate" Width="100"></px:PXGridColumn>
                    <px:PXGridColumn DataField="ShipDate" Width="100"></px:PXGridColumn>
                    <px:PXGridColumn DataField="CustomerID" Width="100"></px:PXGridColumn>
                </Columns>
            </px:PXGridLevel>
        </Levels>
        <AutoSize Container="Window" Enabled="True" MinHeight="150" />
        <ActionBar>
        </ActionBar>
    </px:PXGrid>
    </asp:Content>

Here is the sample graph with the views and the delegate.

    public class MultiInquiry : PXGraph<MultiInquiry>
    {
        public PXCancel<MasterTable> Cancel;
        public PXFilter<MasterTable> MasterView;
        public PXSelect<SOOrder> DetailsView;
    
        public PXSelectJoin<SOOrder, LeftJoin<BAccount, On<SOOrder.customerID, Equal<BAccount.bAccountID>>>, Where<BAccount.acctCD, In<Required<BAccount.acctCD>>>> Orders2;
    
        protected virtual IEnumerable detailsView()
        {
            var list = new List<SOOrder>();
            var customers = MasterView.Current.Customer;
            if (customers != null)
            {
                List<string> customerList = new List<string>();
                customerList.AddRange(customers.Split(new string[] { "; " }, StringSplitOptions.None));
                object[] val = new object[] { customerList.ToArray() };

                foreach (PXResult<SOOrder> res in Orders2.Select(val))
                {
                    SOOrder order = res;
                    list.Add(order);
                }
            }
            return list;
        }
    }
To this we add the DAC containing the definition for the field used in the MultiSelector and the constant for only selecting customer accounts.

        [Serializable]
        public class MasterTable : IBqlTable
        {
            #region InventoryID
            public abstract class customer : IBqlField { }
            [PXString()]
            [PXUIField(DisplayName = "Customer")]
            [PXSelector(typeof(Search<BAccount.acctCD, Where<BAccount.type, Equal<CustomerType>>>), ValidateValue = false)]
            public virtual string Customer { get; set; }
            #endregion

        }

        public class CustomerType : Constant<string> { public CustomerType() : base("CU") { } }

And the result for this example could be something like this : 
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/h7Ryc.jpg

