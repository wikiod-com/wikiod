---
title: "Application ratingreview request"
slug: "application-ratingreview-request"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Now from iOS 10.3 , there is no need to navigate application to apple store for rating/review. Apple has introduced SKStoreReviewController class in storekit framework. In which developer just need to call requestReview() class method of SKStoreReviewController class and the system handles the entire process for you.

In addition, you can continue to include a persistent link in the settings or configuration screens of your app that deep-links to your App Store product page. To automatically ope

## Rate/Review iOS Application
Just type below one line code from where you want user to rate/review your application.

SKStoreReviewController.requestReview()

