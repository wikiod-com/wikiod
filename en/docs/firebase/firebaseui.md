---
title: "FirebaseUI"
slug: "firebaseui"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Firebase is a suite of integrated products designed to help you develop your application, grow an engaged user base, and earn more money. It includes tools that help you build your app, such as a realtime database, file storage, and user authentication, as well as tools to help you grow and monetize your app, such as push notifications, analytics, crash reporting, and dynamic links.

You can think of Firebase as a set of Lego bricks that you can use to build your masterpiece. Just like bricks, Firebase is relatively unopinionated, since there are an infinite number of ways to combine the pieces and we're not going to tell you that certain ways are wrong :)

FirebaseUI is built on Firebase and provides developers simple, customizable, and production-ready native mobile bindings on top of Firebase primitives to eliminate boilerplate code and promote Google best practices

In the Lego analogy, FirebaseUI is a set of pre-built kits with instructions that you can take off the shelf and tweak to suit your needs. You can see how we used the individual components of Firebase to build FirebaseUI because FirebaseUI is open source. FirebaseUI has to be opinionated--we're telling you how we think the bricks should go together, so we make some choices. But because FirebaseUI is open source, you can go in and change what we're doing to better suit your individual needs.

If you're building a Lego city, you'd rather pull a bunch of houses from a pre-build collection and modify slightly to suit your needs than start from scratch and design each building by hand, right?

FirebaseUI let's you do exactly this, which is why we include it in our sample apps and examples. Developers (ourselves included) are lazy--we want the best reuse of our code and the most concise examples, and FirebaseUI allows us to provide really high quality examples that translate to really good user experiences at a fraction of the development cost.

## Getting Started with FirebaseUI
FirebaseUI offers [Android][1], [iOS][2], and [Web][3] clients. You can get started with them like so:

**Android:**

    // app/build.gradle

    dependencies {
        // Single target that includes all FirebaseUI libraries
        compile 'com.firebaseui:firebase-ui:0.5.2'
    
        // FirebaseUI Database only
        compile 'com.firebaseui:firebase-ui-database:0.5.2'
    
        // FirebaseUI Auth only
        compile 'com.firebaseui:firebase-ui-auth:0.5.2'
    }

**iOS:**

    # Podfile

    # Pull in all Firebase UI features
    pod 'FirebaseUI', '~> 0.5'

    # Only pull in the "Database" FirebaseUI features
    pod 'FirebaseUI/Database', '~> 0.5'
    
    # Only pull in the "Auth" FirebaseUI features (including Facebook and Google)
    pod 'FirebaseUI/Auth', '~> 0.5'
    
    # Only pull in the "Facebook" login features
    pod 'FirebaseUI/Facebook', '~> 0.5'
    
    # Only pull in the "Google" login features
    pod 'FirebaseUI/Google', '~> 0.5'

**Web:**

    <!--Include FirebaseUI sources in HTML-->

    <script src="https://www.gstatic.com/firebasejs/ui/live/0.5/firebase-ui-auth.js"></script>
    <link type="text/css" rel="stylesheet" href="https://www.gstatic.com/firebasejs/ui/live/0.5/firebase-ui-auth.css" />


  [1]: https://github.com/firebase/FirebaseUI-Android
  [2]: https://github.com/firebase/FirebaseUI-iOS
  [3]: https://github.com/firebase/FirebaseUI-Web

