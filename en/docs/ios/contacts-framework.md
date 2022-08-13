---
title: "Contacts Framework"
slug: "contacts-framework"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

# Useful Links

* [Apple Documentation][1]

* [Stack Overflow Related Q&A][2]

* [WWDC15 Session Video][3]


  [1]: https://developer.apple.com/library/watchos/documentation/Contacts/Reference/Contacts_Framework/index.html
  [2]: http://stackoverflow.com/questions/32669612/how-to-fetch-all-contacts-record-in-ios-9-using-contacts-framework
  [3]: https://developer.apple.com/videos/play/wwdc2015/223/

## Authorizing Contact Access
# Importing the framework


## Swift

    import Contacts

## Objective-C

    #import <Contacts/Contacts.h>

# Checking accessibility

## Swift

    switch CNContactStore.authorizationStatusForEntityType(CNEntityType.Contacts){
    case .Authorized: //access contacts
    case .Denied, .NotDetermined: //request permission
    default: break
    }

## Objective-C

    switch ([CNContactStore authorizationStatusForEntityType:CNEntityType.Contacts]){
    case CNAuthorizationStatus.Authorized:
        //access contacts
        break;
    case CNAuthorizationStatus.Denied:
        //request permission
        break;
    case CNAuthorizationStatus.NotDetermined:
        //request permission
        break;
    }

# Requesting Permission

## Swift

    var contactStore = CKContactStore()
    contactStore.requestAccessForEntityType(CKEntityType.Contacts, completionHandler: { (ok, _) -> Void in
        if access{
            //access contacts
        }
    }

## Accessing Contacts
# Applying a filter

To access contacts, we should apply a filter of type `NSPredicate` to our contactStore variable which we defined it in Authorizing Contact Access example. For example, here we want to sort out contacts with name matching with our own:

## Swift

    let predicate = CNContact.predicateForContactsMatchingName("Some Name")

## Objective-C

    NSPredicate *predicate = [CNContact predicateForContactsMatchingName:@"Some Name"];

# Specifying keys to fetch

Here, we want to fetch the contact's first name, last name and profile image:

## Swift

    let keys = [CNContactGivenNameKey, CNContactFamilyNameKey, CNContactImageDataKey]

# Fetching contacts

## Swift

    do {
        let contacts = try contactStore.unifiedContactsMatchingPredicate(predicate, keysToFetch: keys)
    } catch let error as NSError {
        //...
    }

# Accessing contact details

## Swift

    print(contacts[0].givenName)
    print(contacts[1].familyName)
    let image = contacts[2].imageData

## Adding a Contact
# Swift

    import Contacts
     
    // Creating a mutable object to add to the contact
    let contact = CNMutableContact()
     
    contact.imageData = NSData() // The profile picture as a NSData object
     
    contact.givenName = "John"
    contact.familyName = "Appleseed"
     
    let homeEmail = CNLabeledValue(label:CNLabelHome, value:"john@example.com")
    let workEmail = CNLabeledValue(label:CNLabelWork, value:"j.appleseed@icloud.com")
    contact.emailAddresses = [homeEmail, workEmail]
     
    contact.phoneNumbers = [CNLabeledValue(
        label:CNLabelPhoneNumberiPhone,
        value:CNPhoneNumber(stringValue:"(408) 555-0126"))]
     
    let homeAddress = CNMutablePostalAddress()
    homeAddress.street = "1 Infinite Loop"
    homeAddress.city = "Cupertino"
    homeAddress.state = "CA"
    homeAddress.postalCode = "95014"
    contact.postalAddresses = [CNLabeledValue(label:CNLabelHome, value:homeAddress)]
     
    let birthday = NSDateComponents()
    birthday.day = 1
    birthday.month = 4
    birthday.year = 1988  // You can omit the year value for a yearless birthday
    contact.birthday = birthday
     
    // Saving the newly created contact
    let store = CNContactStore()
    let saveRequest = CNSaveRequest()
    saveRequest.addContact(contact, toContainerWithIdentifier:nil)
    try! store.executeSaveRequest(saveRequest)

