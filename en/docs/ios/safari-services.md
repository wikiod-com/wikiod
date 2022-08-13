---
title: "Safari Services"
slug: "safari-services"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Open a URL with SafariViewController
Don't forget to import the necessary framework first.

    import SafariServices
    //Objective-C
    @import SafariServices;

Instantiate a `SafariViewController` instance.

    let safariVC = SFSafariViewController(URL: URL(string: "your_url")!)
    //Objective-C
    @import SafariServices;
    NSURL *URL = [NSURL URLWithString:[NSString stringWithFormat:@"http://www.google.com"]];
    SFSafariViewController *sfvc = [[SFSafariViewController alloc] initWithURL:URL];


Optionally you can also tell SafariViewController to enter reading mode if possible once it's done loading.

    let safariVC = SFSafariViewController(URL: URL(string: "your_url")!, entersReaderIfAvailable: true)
    //Objective-C
    NSURL *URL = [NSURL URLWithString:[NSString stringWithFormat:@"http://www.google.com"]];
    SFSafariViewController *sfvc = [[SFSafariViewController alloc] initWithURL:URL entersReaderIfAvailable:YES];

Present the view controller.

    present(safariVC, animated: true, completion: nil)
    //Objective-C
    [self presentViewController:sfvc animated:YES completion:nil];

## Implement SFSafariViewControllerDelegate
You should implement `SFSafariViewControllerDelegate` so that your class is notified when the user hits the Done button on the SafariViewController and you can dismiss it as well.

First declare your class to implement the protocol.

    class MyClass: SFSafariViewControllerDelegate {
    
    }

Implement the delegate method to be notified on dismissal.

    func safariViewControllerDidFinish(controller: SFSafariViewController) {
        // Dismiss the SafariViewController when done
        controller.dismissViewControllerAnimated(true, completion: nil)
    }

Don't forget to set your class as the SafariViewController's delegate.

    let safariVC = SFSafariViewController(URL: yourURL)
    safariVC.delegate = self


Additional delegate methods you can implement are:

    // Called when the initial URL load is complete.
    safariViewController(_ controller: SFSafariViewController, didCompleteInitialLoad didLoadSuccessfully: Bool) { }

    // Called when the user taps an Action button.
    safariViewController(_ controller: SFSafariViewController, activityItemsFor URL: URL, title: String?) -> [UIActivity] { }

## Add Items to Safari Reading List
You can add items to a user's Reading List in Safari by calling the `addItem` method on the `SSReadingList` singleton.

    let readingList = SSReadingList.default()
    readingList?.addItem(with: yourURL, title: "optional title", previewText: "optional preview text")

The default Reading List can be `nil` if access to the Reading List is not permitted.

Additionally you can check if the Reading List supports a URL by calling `supportsURL`.

    SSReadingList.default().supportsURL(URL(string: "https://example.com")!)

This will return either `true` or `false` indicating if the given URL is supported by Safari Reading List. Use this for example to determine whether to show a button to add a URL to the Reading List.

