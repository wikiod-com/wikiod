---
title: "Displaying an Error Requiring to Enter Entity Data"
slug: "displaying-an-error-requiring-to-enter-entity-data"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Displaying an Error Requiring the User to Enter Entity Data
Users often turn up in a situation when a business process cannot be finished because the user has not entered all the necessary information.

An example of this situation is when a user tries to create a drop-ship order with missing customer address.

According to UX best practices, the system should be friendly to the user and not only inform the user about the situation, but also guide him to the resolution of his issue. As we know, the system already has a similar mechanism activated by `PXSetup<TSetup>.Current` when there are no records in the `TSetup` table. It is internally implemented by throwing a `PXSetupNotEnteredException`.

Recently, a new functionality has been added to this exception, which allows an application developer to throw an error with a link to the entity which must be re-configured:

```
INSite erroneousSite = PXSelect<
    INSite,
    Where<
        INSite.siteID, Equal<Current<SOCreateFilter.siteID>>, 
        And<INSite.active, Equal<True>, 
        And<Where<INSite.addressID, IsNull, Or<INSite.contactID, IsNull>>>>>>
    .SelectSingleBound(this, new object[] { e.Row });

if (erroneousSite != null)
{
    throw new PXSetupNotEnteredException<INSite, INSite.siteCD>(
        Messages.WarehouseWithoutAddressAndContact, 
        erroneousSite.SiteCDlnk, 
        erroneousSite.SiteCDinf);
}
```

The result is displayed to the user like this:

[![error page look][1]][1]

* As a first type parameter, `PXSetupNotEnteredException` accepts the type of the entity to which the default graph link will be generated.
* The second type parameter denotes the key field of the record to be used to generate the link. In the above example, navigation to the warehouse entity is made by the CD key.
* The first constructor argument is the format string for the error message. The numbering of its internal placeholders should start with 1: i.e. `The Multiple Warehouses feature and the Transfer order type are activated in the system, in this case an address and a contact must be configured for the '{1}' warehouse.`
* The second constructor argument is the value of the key field specified as the second generic parameter. In the example, the link that would be generated is `/IN204000.aspx?siteCD=erroneousSite.SiteCDlnk`.
* The third constructor argument is the human-readable value to be displayed in the error message: `...in this case an address and a contact must be configured for the 'erroneousSite.SiteCDinf' warehouse.`

  [1]: https://i.stack.imgur.com/Cgbmv.png

