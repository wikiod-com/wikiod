---
title: "Engagement Automation"
slug: "engagement-automation"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Sitecore automation gives marketer a possobility to create marketing workflows which will put user through different states on the website.

The example of automation usage could be registration workflow (registered, confirmed, logged in) or purchase workflow (new order, added products, payment details, purchase complete).

## Get the contact by username
    ContactManager contactManager = Factory.CreateObject("tracking/contactManager", true) as ContactManager;
        
    Contact contact = contactManager.LoadContactReadOnly(userName);
    return contact;

## Change the automation state of the contact
This method doesn't require initialization of the tracker, which is handy if the state should be changed outside of the site context (for example in the shell).

    var stateManager = AutomationStateManager.Create(contact);
    automationStateManager.MoveToEngagementState(stateItem.ParentID, stateId);
    stateManager.SaveChanges(AutomationManager.Provider);

## Enroll contact in the engagement plan programmatically
     AutomationMetadataProvider automationMetadataProvider = Assert.ResultNotNull(Factory.CreateObject("automation/metadataProvider", true) as AutomationMetadataProvider);            
     var context = AutomationManager.Provider.GetAutomationContext(ID.Parse(contact.ContactId));            
     context.Enroll(fromStateItem.ParentID, ID.Parse(fromState), automationMetadataProvider.CalculateWakeUpDateTime(ID.Parse(fromState), DateTime.UtcNow), null);
     AutomationManager.Provider.SaveAutomationContext(context);


