---
title: "Determine if private database is available"
slug: "determine-if-private-database-is-available"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| completionHandler   | A block that has no return value and takes the following parameters:   |
| accountStatus |  The status of the current user’s iCloud account.|
| error | An error object or nil if the status is determined successfully. Use the information in the error object to determine whether the problem has a workaround. |


Account status returned can be: couldNotDetermine, available, restricted or noAccount.

The status can change while the app is running, use the `NSUbiquityIdentityDidChangeNotification` to detect account changes and call this method again to retrieve the status for the new account.

## accountStatusWithCompletionHandler
    CKContainer.defaultContainer().accountStatusWithCompletionHandler { accountStatus, error in
            if accountStatus == .NoAccount {
                let alert = UIAlertController(title: "Sign in to iCloud", message: "Sign in to your iCloud account to write records. On the Home screen, launch Settings, tap iCloud, and enter your Apple ID. Turn iCloud Drive on. If you don't have an iCloud account, tap Create a new Apple ID.", preferredStyle: .Alert)
                alert.addAction(UIAlertAction(title: "OK", style: .Default, handler: nil))
                self.presentViewController(alert, animated: true, completion: nil)
            } else {
                // User has access to private database here...
            }
        }



