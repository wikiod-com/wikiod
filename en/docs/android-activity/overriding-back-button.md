---
title: "overriding back button"
slug: "overriding-back-button"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Double back to close application
You may have noticed that many applications have double-back-click functionality to exit the app. In this example, we are overriding the default back button action using the `onBackPressed()` method override. 

This method will `Toast` a message for the single back-click action, and will close the app if the user clicks the back button twice within two seconds. We use a [`Handler`][1] for the functionality that relies on the two-second interval.




    boolean singleBack = false;

    @Override
    public void onBackPressed() {
       if (singleBack) {
        super.onBackPressed();
        return;
       }

       this.singleBack = true;
       Toast.makeText(this, "Double Back to exit", Toast.LENGTH_SHORT).show();

       new Handler().postDelayed(new Runnable() {

          @Override
          public void run() {
              singleBack=false;                       
          }
       }, 2000);
    } 


  [1]: https://www.wikiod.com/android/handler

