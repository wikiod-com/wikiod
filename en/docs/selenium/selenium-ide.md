---
title: "Selenium IDE"
slug: "selenium-ide"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Try a simple Selenium script: Search Wikipedia on Google
Prerequisites:

 - Install Firefox
 - Install Selenium IDE addon (https://addons.mozilla.org/fr/firefox/addon/selenium-ide/)

Open the plugin. A button displaying a red circle must be shown. If it's pressed, it means you can start your scenario. The plugin is recording everything you do **within this Firefox instance**.<br>
Do whatever you want to be recorded.<br>
In the end, save your scenario; you will notice that Selenium IDE's scenarios are html files.<br>
You can also open files from other users. For instance, copy and paste the code below in a new html file and import it via your plugin. You will be able to run the -very simple- scenario.

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
    <head profile="http://selenium-ide.openqa.org/profiles/test-case">
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <link rel="selenium.base" href="https://www.google.com/" />
    <title>sample-test</title>
    </head>
    <body>
    <table cellpadding="1" cellspacing="1" border="1">
    <thead>
    <tr><td rowspan="1" colspan="3">sample-test</td></tr>
    </thead><tbody>
    <tr>
        <td>open</td>
        <td>/</td>
        <td></td>
    </tr>
    <tr>
        <td>type</td>
        <td>id=lst-ib</td>
        <td>Wikipedia</td>
    </tr>
    </tbody></table>
    </body>
    </html>

This DSL (domain specific language) is commonly named "selenese".<br>
Its most common functions are listed <a href="http://toolsqa.com/selenium-ide/selenium-ide-commands/">here</a>.<br>

