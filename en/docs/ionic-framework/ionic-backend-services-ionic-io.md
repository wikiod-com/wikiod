---
title: "Ionic Backend Services (ionic.io)"
slug: "ionic-backend-services-ionicio"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Introduction and setup
The [Ionic Platform][1] offers a range of powerful, hybrid-focused mobile backend services and tools to make it easy to scale beautiful, performant hybrid apps, at a rapid pace.

In order to use Ionic Platform you need to have the [Ionic Framework
installed][2].

1.) **Registration** ([sign-up][3])

You need to enter: name, username, company, phone (optional), email and password. After you receive confirmation email you may [log-in][4]. 

> Welcome to Ionic. Let's create your first app! Create a new app above,
> then follow the [Quick Start][5] guide to upload your first app.

2.) **Setup** 

First, [create your firs Ionic app][6]. Now you can add the platform web client: 

> The web client gives you a way to interact with the Ionic Platform
> services from within your app code.

    $ ionic add ionic-platform-web-client

Now, you need the Platform to assign your app a unique app id and api key. To do that use the `ionic io init` command.

    $ ionic io init

> This will automatically prompt you to login to your Platform account.
> The app id and api key will then be stored in your project's Ionic
> Platform [config][7].
> 
> Your app is now hooked up to the Ionic Platform and will be listed in
> your [Dashboard][8].

3.) **Now, you can move on to installing one of Ionic Platform services**:

 - [Users / Auth][9]
 - [Deploy][10]
 - [Push][11]
 - [Package][12]
 - [Analytics][13]


  [1]: http://ionic.io/
  [2]: https://www.wikiod.com/ionic-framework/getting-started-with-ionic-framework#Installation or Setup
  [3]: https://apps.ionic.io/signup
  [4]: https://apps.ionic.io/login
  [5]: http://docs.ionic.io/docs/io-quick-start
  [6]: https://www.wikiod.com/ionic-framework/getting-started-with-ionic-framework#Installation or Setup
  [7]: http://docs.ionic.io/v2.0.0-beta/docs/io-config
  [8]: https://apps.ionic.io/apps
  [9]: http://docs.ionic.io/docs/user-overview
  [10]: http://docs.ionic.io/docs/deploy-overview
  [11]: http://docs.ionic.io/docs/push-overview
  [12]: http://docs.ionic.io/docs/package-overview
  [13]: http://docs.ionic.io/docs/analytics-auto-tracking

