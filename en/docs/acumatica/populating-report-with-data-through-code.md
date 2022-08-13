---
title: "Populating report with data through code"
slug: "populating-report-with-data-through-code"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## This article covers example showing how to create report using memory records:
This example shows how to populate report with data returned by a data view delegate. During the exercise, we will develop an inquiry screen showing list of Sales Orders between two dates. Data view delegate will be used to populate Sales Order information.

**Prerequisites:**
 1. We start with declaration of the SOOrderFilter DAC:

        [Serializable]
        public class SOOrderFilter : IBqlTable
        {
            public abstract class dateFrom : IBqlField
            {
            }
            [PXDate()]
            [PXUIField(DisplayName = "Date From")]
            public DateTime? DateFrom { get; set; }

            public abstract class dateTo : IBqlField
            {
            }
            [PXDate()]
            [PXUIField(DisplayName = "Date To")]
            public DateTime? DateTo { get; set; }
        }

 2. Continue with declaration of the SOOrderData DAC:

        [Serializable]
        public class SOOrderData : IBqlTable
        {
            #region OrderType
            public abstract class orderType : PX.Data.IBqlField
            {
            }
            [PXString(2, IsKey = true, IsFixed = true)]
            [PXUIField(DisplayName = "Type")]
            public virtual string OrderType { get; set; }
            #endregion
            #region OrderNbr
            public abstract class orderNbr : PX.Data.IBqlField
            {
            }
            [PXString(15, IsKey = true, IsUnicode = true, InputMask = ">CCCCCCCCCCCCCCC")]
            [PXUIField(DisplayName = "Order Nbr.")]
            public virtual string OrderNbr { get; set; }
            #endregion
            #region OrderDate
            public abstract class orderDate : PX.Data.IBqlField
            {
            }
            [PXDate]
            [PXUIField(DisplayName = "Date")]
            public virtual DateTime? OrderDate { get; set; }
            #endregion
            #region Status
            public abstract class status : PX.Data.IBqlField
            {
            }
            [PXString(1, IsFixed = true)]
            [PXUIField(DisplayName = "Status")]
            [SOOrderStatus.List()]
            public virtual string Status { get; set; }
            #endregion
            #region OrderDesc
            public abstract class orderDesc : PX.Data.IBqlField
            {
            }
            [PXString(60, IsUnicode = true)]
            [PXUIField(DisplayName = "Description", Visibility = PXUIVisibility.SelectorVisible)]
            public virtual string OrderDesc { get; set; }
            #endregion
            #region OrderTotal
            public abstract class orderTotal : PX.Data.IBqlField
            {
            }
            [PXDecimal(4)]
            [PXDefault(TypeCode.Decimal, "0.0")]
            public virtual decimal? OrderTotal { get; set; }
            #endregion
            #region DueDate
            public abstract class dueDate : PX.Data.IBqlField
            {
            }
            [PXDate]
            [PXUIField(DisplayName = "Due Date")]
            public virtual DateTime? DueDate { get; set; }
            #endregion
        }

 3. In PX.Documentation namespace сreate your SOOrderInq BLC using the code snippet below to declare Results data view delegate, which will later use to populate report with data:

        public class SOOrderInq : PXGraph<SOOrderInq>
        {
            public PXCancel<SOOrderFilter> Cancel;
            public PXFilter<SOOrderFilter> Filter;
    
            [PXFilterable]
            public PXSelectOrderBy<SOOrderData,
                OrderBy<Desc<SOOrderData.orderNbr>>> Result;
            protected virtual IEnumerable result()
            {
                BqlCommand cmd = PXSelect<SOOrder, 
                    Where<SOOrder.orderDate,
                        Between<Current<SOOrderFilter.dateFrom>,
                            Current<SOOrderFilter.dateTo>>>>.GetCommand();
                PXView inView = new PXView(this, true, cmd);
                int startRow = PXView.StartRow;
                int totalRows = 0;
                foreach (SOOrder order in inView.Select(PXView.Currents, PXView.Parameters,
                    PXView.Searches, PXView.SortColumns, PXView.Descendings, PXView.Filters,
                    ref startRow, PXView.MaximumRows, ref totalRows))
                {
                    yield return new SOOrderData
                    {
                        OrderType = order.OrderType,
                        OrderNbr = order.OrderNbr,
                        OrderDate = order.OrderDate,
                        Status = order.Status,
                        OrderDesc = order.OrderDesc,
                        OrderTotal = order.OrderTotal,
                        DueDate = order.DueDate,
                    };
                }
                PXView.StartRow = 0;
            }
    
            public SOOrderInq()
            {
                Result.Cache.AllowInsert = false;
                Result.Cache.AllowUpdate = false;
                Result.Cache.AllowDelete = false;
            }
    
            public PXAction<SOOrderFilter> Report;
            [PXButton]
            [PXUIField(DisplayName = "View As Report", MapEnableRights = PXCacheRights.Select, MapViewRights = PXCacheRights.Select)]
            protected virtual void report()
            {
                PXReportResultset reportData = new PXReportResultset(typeof(SOOrderData));
                foreach (SOOrderData row in Result.Select())
                {
                    reportData.Add(row);
                }
                throw new PXReportRequiredException(reportData, "SO610501", PXBaseRedirectException.WindowMode.NewWindow, "Report");
            }
        }


 4. Create SO401090.aspx page by selecting FormDetail template, and set the following properties for PXDataSource:

 - PrimaryView: Filter
 - TypeName: PX.Documentation.SOOrderInq

    After that add input control on the Filter header form:

        <px:PXFormView ID="form" runat="server" DataSourceID="ds" Style="z-index: 100" 
            Width="100%" DataMember="Filter">
            <Template>
                <px:PXLayoutRule runat="server" StartRow="True" Merge="True" LabelsWidth="XS" ControlSize="S" />
                <px:PXDateTimeEdit ID="edDateFrom" runat="server" CommitChanges="True" DataField="DateFrom" />
                <px:PXDateTimeEdit ID="edDateTo" runat="server" CommitChanges="True" DataField="DateTo" />
                <px:PXLayoutRule runat="server" />
            </Template>
        </px:PXFormView>

    And create the following columns for the Detail grid:

        <px:PXGrid ID="grid" runat="server" DataSourceID="ds" Style="z-index: 100" 
            Width="100%" Height="150px" SkinID="Inquire" AllowPaging="True" AdjustPageSize="Auto">
            <Levels>
                <px:PXGridLevel DataMember="Result">
                    <Columns>
                        <px:PXGridColumn DataField="OrderType" />
                        <px:PXGridColumn DataField="OrderNbr" Width="90px" />
                        <px:PXGridColumn DataField="OrderDate" Width="90px" />
                        <px:PXGridColumn DataField="Status" />
                        <px:PXGridColumn DataField="OrderDesc" Width="200px" />
                        <px:PXGridColumn DataField="DueDate" Width="90px" />
                    </Columns>
                </px:PXGridLevel>
            </Levels>
            <AutoSize Container="Window" Enabled="True" MinHeight="150" />
        </px:PXGrid>

 5. Add created screen to the Site Map

**To populate report with data returned by a data view delegate:**
 1. Paste [SO610501.rpx][1] report file in ReportsCustomized folder of your Acumatica website, then add Sales Orders report in the Site Map **Hidden** folder

[![enter image description here][2]][2]

 2. Declare **View as Report** action in the SOOrderInq BLC to generate and show Sales Orders report. The PXReportRequiredException accepts PXReportResultset prepared inside the action to populate report with data returned by **Result** data view delegate:

        public class SOOrderInq : PXGraph<SOOrderInq>
        {
            ...

            public PXAction<SOOrderFilter> Report;
            [PXButton]
            [PXUIField(DisplayName = "View as Report", MapEnableRights = PXCacheRights.Select, MapViewRights = PXCacheRights.Select)]
            protected virtual void report()
            {
                PXReportResultset reportData = new PXReportResultset(typeof(SOOrderData));
                foreach (SOOrderData row in Result.Select())
                {
                    reportData.Add(row);
                }
                throw new PXReportRequiredException(reportData, "SO610501", PXBaseRedirectException.WindowMode.NewWindow, "Report");
            }
        }

    [![enter image description here][3]][3]


  [1]: https://drive.google.com/file/d/0B5OI3IlOznP8dXVoYkN1aXJlT28/view?usp=sharing
  [2]: https://i.stack.imgur.com/6CVI4.png
  [3]: https://i.stack.imgur.com/zQXj2.png

