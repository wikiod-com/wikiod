---
title: "Changing caption dynamically using readonly DAC fields."
slug: "changing-caption-dynamically-using-readonly-dac-fields"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

This example shows how to change dynamically the Caption/Label of Customer Name field on Customer ScreenID AR303000 on Acumatica ERP, depending on current Customer ID selected on the same form. We could:

## How-To


**Add new unbound field to the DAC. (as readonly)**

  

      [PXString(60, IsUnicode = true)]
      [PXUIField(Enabled = false, IsReadOnly = true)]
      public virtual string UsrReadOnlyAcctName{get;set;}
      public abstract class usrReadOnlyAcctName : IBqlField{}

**Modify its value depending on conditions using handlers. (On Customer cycle ID Selected)**

    public class CustomerMaint_Extension:PXGraphExtension<CustomerMaint>
      {
        protected void Customer_RowSelected(PXCache sender, PXRowSelectedEventArgs e)
          {
              var customer = (BAccount)e.Row;
              var customerExt = customer.GetExtension<BAccountExt>();
              if (customerExt != null)
              {
                  customerExt.UsrReadOnlyAcctName = customer.AcctName;
              }
          }
      }

**SuppressLabel(true) for both new unbound fields and existing fields whose label will be replace.**

[![enter image description here][1]][1]


**Place the added unbound field before the existing field.**

Results:

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/rljEm.png
  [2]: https://i.stack.imgur.com/2B3Hm.png

