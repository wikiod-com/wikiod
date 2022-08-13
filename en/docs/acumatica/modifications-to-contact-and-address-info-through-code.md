---
title: "Modifications to Contact and Address Info through Code"
slug: "modifications-to-contact-and-address-info-through-code"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

In this topic, you will learn how to modify Contact and Address information through code on different screens inside Acumatica.

## Specify Contact and Address information for a new Employee
To specify Contact and Address info for an Employee, you should always invoke `Select()` method on the **Contact** and **Address** data views prior to assigning any field values. It is also recommended to assign the result of `Select()` method to the **Contact** and **Address** data views' ***Current*** property to guarantee that your code modifies the current record in **Contact** and **Address** PXCache respectively.

    EmployeeMaint employeeMaintGraph = PXGraph.CreateInstance<EmployeeMaint>();
    EPEmployee epEmployeeRow = new EPEmployee();
    epEmployeeRow.AcctCD = "EMPLOYEE1";
    epEmployeeRow = employeeMaintGraph.Employee.Insert(epEmployeeRow);

    Contact contactRow = employeeMaintGraph.Contact.Current = employeeMaintGraph.Contact.Select();
    contactRow.FirstName = "John";
    contactRow.LastName = "Green";
    employeeMaintGraph.Contact.Update(contactRow);

    Address addressRow = employeeMaintGraph.Address.Current = employeeMaintGraph.Address.Select();
    addressRow.CountryID = "US";
    addressRow = employeeMaintGraph.Address.Update(addressRow);
    addressRow.State = "DC";
    employeeMaintGraph.Address.Update(addressRow);

    epEmployeeRow.VendorClassID = "EMPSTAND";
    epEmployeeRow.DepartmentID = "FINANCE";
    employeeMaintGraph.Employee.Update(epEmployeeRow);

    employeeMaintGraph.Actions.PressSave();

When inserting a new Employee, `employeeMaintGraph.Contact.Current` will always return the main contact record as the contact record gets automatically inserted into the cache and therefore becomes available via the ***Current*** property of PXCache/Data View. The use of `Select()` method is a little more generic since it will work in all possible scenarios, whether you need to insert new Employee or update an existing one.

## Override Bill-To Contact and Bill-To Address Info for a Customer
When you need to override Bill-To Contact and Bill-To Address info for a Customer, the very first step is to set correct values for the ***IsBillContSameAsMain*** and ***IsBillSameAsMain*** properties of the **Customer** DAC. Don't forget to invoke `Update()` method on the **Customer** cache right after you updated ***IsBillContSameAsMain*** or ***IsBillSameAsMain*** property to commit the current ***Same as Main*** field value into the cache. 

Your next step is to invoke `Select()` method on the **BillContact** and **BillAddress** data views prior to assigning any field values. It is also recommended to assign the result of `Select()` method to the **BillContact** and **BillAddress** data views' ***Current*** property to guarantee that your code modifies the current record in **Contact** and **Address** PXCache respectively.

    public class CustomerMaintExt : PXGraphExtension<CustomerMaint>
    {
        public PXAction<Customer> UpdateBillingAddress;
        [PXButton(CommitChanges = true)]
        [PXUIField(DisplayName = "Update Bill-To Info")]
        protected void updateBillingAddress()
        {
            Customer currentCustomer = Base.BAccount.Current;

            if (currentCustomer.IsBillContSameAsMain != true)
            {
                currentCustomer.IsBillContSameAsMain = true;
                Base.BAccount.Update(currentCustomer);
            }
            else
            {
                currentCustomer.IsBillContSameAsMain = false;
                Base.BAccount.Update(currentCustomer);

                Contact billContact = Base.BillContact.Current = Base.BillContact.Select();
                billContact.FullName = "ABC Holdings Inc";
                billContact.Phone1 = "+1 (212) 532-9574";
                Base.BillContact.Update(billContact);
            }

            if (currentCustomer.IsBillSameAsMain != true)
            {
                currentCustomer.IsBillSameAsMain = true;
                Base.CurrentCustomer.Update(currentCustomer);
            }
            else
            {
                currentCustomer.IsBillSameAsMain = false;
                Base.CurrentCustomer.Update(currentCustomer);

                Address billAddress = Base.BillAddress.Current = Base.BillAddress.Select();
                billAddress.AddressLine1 = "65 Broadway";
                billAddress.AddressLine2 = "Office Suite 187";
                billAddress.City = "New York";
                billAddress.CountryID = "US";
                billAddress = Base.BillAddress.Update(billAddress);
                billAddress.State = "NY";
                billAddress.PostalCode = "10004";
                Base.BillAddress.Update(billAddress);
            }

            Base.Actions.PressSave();
        }
    }

## Override Bill-To Contact and Bill-To Address Info for a Sales Order
To specify Bill-To Contact and Bill-To Address info for a Sales Order, you should always first invoke `Select()` method on the **Billing_Contact** and **Billing_Address** data views prior to assigning any field values. It is also recommended to assign the result of `Select()` method to the **Billing_Contact** and **Billing_Address** data views' ***Current*** property to guarantee that your code modifies the current record in **SOBillingContact** and **SOBillingAddress** PXCache respectively.

When you need to override Bill-To Contact and Address info for a Sales Order, set correct values for the ***OverrideContact*** and ***OverrideAddress*** properties on the **SOBillingContact** DAC and the **SOBillingAddress** DAC. Don't forget to invoke `Update()` method on the **SOBillingContact** and **SOBillingAddress** caches right after you updated ***OverrideContact*** and ***OverrideAddress*** properties to commit the changes. Once that's done, you can go ahead and specify Bill-To Contact and Address info for a Sales Order.

    public class SOOrderEntryExt : PXGraphExtension<SOOrderEntry>
    {
        public PXAction<SOOrder> UpdateBillingAddress;
        [PXButton(CommitChanges = true)]
        [PXUIField(DisplayName = "Update Bill-To Info")]
        protected void updateBillingAddress()
        {
            SOBillingContact contact = Base.Billing_Contact.Current = Base.Billing_Contact.Select();
            if (contact.OverrideContact == true)
            {
                contact.OverrideContact = false;
                Base.Billing_Contact.Update(contact);
            }
            else
            {
                contact.OverrideContact = true;
                contact = Base.Billing_Contact.Update(contact);
                if (contact == null)
                {
                    contact = Base.Billing_Contact.Current;
                }

                contact.Phone1 = "+1 (908) 643-0281";
                contact.Email = "sales@usabartend.con";
                Base.Billing_Contact.Update(contact);
            }

            SOBillingAddress address = Base.Billing_Address.Current = Base.Billing_Address.Select();
            if (address.OverrideAddress == true)
            {
                address.OverrideAddress = false;
                Base.Billing_Address.Update(address);
            }
            else
            {
                address.OverrideAddress = true;
                address = Base.Billing_Address.Update(address);
                if (address == null)
                {
                    address = Base.Billing_Address.Current;
                }

                address.AddressLine1 = "201 Lower Notch Rd";
                address.AddressLine2 = "Office Suite 1936";
                address.City = "Little Falls";
                address.CountryID = "US";
                address = Base.Billing_Address.Update(address);
                address.State = "NJ";
                address.PostalCode = "07425";
                Base.Billing_Address.Update(address);
            }

            Base.Actions.PressSave();
        }
    }

