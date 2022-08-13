---
title: "Modifications to Base Data Views"
slug: "modifications-to-base-data-views"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

This topic is intended to demonstrate various patterns and practices available to modify base data views in Acumatica.

## APInvoiceEntry BLC: add additional restriction to poReceiptLinesSelection data view
To add additional restriction to the **poReceiptLinesSelection** data view, you should  invoke `Select` method on base view and inspect each item in returned `PXResultSet` independently to decide if it complies with additional restriction(s):

    public class APInvoiceEntryExt : PXGraphExtension<APInvoiceEntry>
    {
        [PXCopyPasteHiddenView]
        public PXSelect<APInvoiceEntry.POReceiptLineAdd> poReceiptLinesSelection;

        public virtual IEnumerable POReceiptLinesSelection()
        {
            foreach (var record in Base.poReceiptLinesSelection.Select())
            {
                // Additional restriction goes here
                if (true == true)
                {
                    yield return record;
                }
            }
        }
    }

This approach perfectly works with the **poReceiptLinesSelection** data view, due to lack of paging and aggregation in the implementation of base view. To compose result set, **poReceiptLinesSelection** view requests necessary data from database and performs all calculations on the application side.

    public class APInvoiceEntry : APDataEntryGraph<APInvoiceEntry, APInvoice>, PXImportAttribute.IPXPrepareItems
    {
        ...

        [PXCopyPasteHiddenView]
        public PXSelect<POReceiptLineAdd> poReceiptLinesSelection;

        public virtual IEnumerable POReceiptLinesSelection()
        {
            APInvoice doc = this.Document.Current;
            if (doc == null || doc.VendorID == null || doc.VendorLocationID == null) yield break;
            if (doc.DocType != APDocType.Invoice && doc.DocType != APDocType.DebitAdj)
                yield break;

            string poReceiptType = (doc.DocType == APDocType.Invoice) ? POReceiptType.POReceipt : POReceiptType.POReturn;

            HashSet<APTran> usedRecceiptLine = new HashSet<APTran>(new POReceiptLineComparer());
            HashSet<APTran> unusedReceiptLine = new HashSet<APTran>(new POReceiptLineComparer());

            foreach (APTran aPTran in Transactions.Cache.Inserted)
            {
                if (aPTran.ReceiptNbr != null && aPTran.ReceiptType != null && aPTran.ReceiptLineNbr != null)
                    usedRecceiptLine.Add(aPTran);
            }

            foreach (APTran aPTran in Transactions.Cache.Deleted)
            {
                if (aPTran.ReceiptNbr != null && aPTran.ReceiptType != null && aPTran.ReceiptLineNbr != null && Transactions.Cache.GetStatus(aPTran) != PXEntryStatus.InsertedDeleted)
                    if (!usedRecceiptLine.Remove(aPTran))
                        unusedReceiptLine.Add(aPTran);
            }

            foreach (APTran aPTran in Transactions.Cache.Updated)
            {
                APTran originAPTran = new APTran();
                originAPTran.ReceiptNbr = (String)Transactions.Cache.GetValueOriginal<APTran.receiptNbr>(aPTran);
                originAPTran.ReceiptType = (String)Transactions.Cache.GetValueOriginal<APTran.receiptType>(aPTran);
                originAPTran.ReceiptLineNbr = (Int32?)Transactions.Cache.GetValueOriginal<APTran.receiptLineNbr>(aPTran);

                if (originAPTran.ReceiptNbr != null && originAPTran.ReceiptType != null && originAPTran.ReceiptLineNbr != null)
                {
                    if (!usedRecceiptLine.Remove(originAPTran))
                        unusedReceiptLine.Add(originAPTran);
                }

                if (aPTran.ReceiptNbr != null && aPTran.ReceiptType != null && aPTran.ReceiptLineNbr != null)
                {
                    if (!unusedReceiptLine.Remove(aPTran))
                        usedRecceiptLine.Add(aPTran);
                }
            }

            foreach (LinkLineReceipt item in PXSelect<LinkLineReceipt,
                Where<LinkLineReceipt.vendorID, Equal<Current<APInvoice.vendorID>>,
                And<LinkLineReceipt.vendorLocationID, Equal<Current<APInvoice.vendorLocationID>>,
                And<LinkLineReceipt.receiptCuryID, Equal<Current<APInvoice.curyID>>,
                And<LinkLineReceipt.receiptType, Equal<Required<POReceipt.receiptType>>,
                And<Where<LinkLineReceipt.orderNbr, Equal<Current<POReceiptFilter.orderNbr>>, Or<Current<POReceiptFilter.orderNbr>, IsNull>>>
                >>>>>.SelectMultiBound(this, new object[] { doc }, poReceiptType))
            {
                APTran aPTran = new APTran();
                aPTran.ReceiptType = item.ReceiptType;
                aPTran.ReceiptNbr = item.ReceiptNbr;
                aPTran.ReceiptLineNbr = item.ReceiptLineNbr;
                if (!usedRecceiptLine.Contains(aPTran))
                    yield return (PXResult<POReceiptLineAdd, POReceipt>)ReceipLineAdd.Select(item.ReceiptNbr, item.ReceiptType, item.ReceiptLineNbr);
            }

            foreach (APTran item in unusedReceiptLine)
            {
                yield return (PXResult<POReceiptLineAdd, POReceipt>)ReceipLineAdd.Select(item.ReceiptNbr, item.ReceiptType, item.ReceiptLineNbr);
            }

        }

        ...
    }

