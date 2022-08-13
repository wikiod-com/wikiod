---
title: "Security"
slug: "security"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Sitecore offers two ways to access items that the context user doesn't have permissions to access. The preferred way is to use the UserSwitcher class to temporarily change the user that will be used to access the item. The reason that this is preferred is because you can still have permissions in effect for the user account that is being utilised. 

The alternative is to use the SecurityDisabler class. This performs the action without any security constraints.

It is recommended to only use these classes for the operations that require elevated permissions. The best way to ensure this is by utilising the 'using' keyword in C#; this will ensure that the UserSwitcher / SecurityDisabler is correctly disposed.

## Disable permission checking when accessing an item
    using (new Sitecore.SecurityModel.SecurityDisabler())
    {
        var item = Sitecore.Context.Database.GetItem("/sitecore/content/home");               
    }

## Impersonate a different user when accessing an item
    var user = Sitecore.Security.Accounts.User.FromName("sitecore/testname", false); 

    using (new Sitecore.Security.Accounts.UserSwitcher(user))
    {
        var item = Sitecore.Context.Database.GetItem("/sitecore/content/home");
    }

