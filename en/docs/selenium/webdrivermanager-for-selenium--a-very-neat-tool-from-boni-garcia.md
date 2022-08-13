---
title: "WebDriverManager for Selenium - a very neat tool from Boni Garcia"
slug: "webdrivermanager-for-selenium---a-very-neat-tool-from-boni-garcia"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

I switched to Selenium 3 and started using Chrome instead of Firefox. I discovered that for Chrome I need to download a binary for WebDriver to handle the browser. For that to work I need to set absolute path to this binary as Java variable. If binary gets updated, I need to update that binary manually in my test framework - which takes time and is really annoying. I discovered a very neat Java library that does it for me: https://github.com/bonigarcia/webdrivermanager

## Below examples shows how easy it is to use
    ChromeDriverManager.getInstance().setup();
    FirefoxDriverManager.getInstance().setup();
    OperaDriverManager.getInstance().setup();
    PhantomJsDriverManager.getInstance().setup();
    EdgeDriverManager.getInstance().setup();
    InternetExplorerDriverManager.getInstance().setup();

