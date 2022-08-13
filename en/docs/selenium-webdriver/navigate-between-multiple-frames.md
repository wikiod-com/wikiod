---
title: "Navigate between multiple frames"
slug: "navigate-between-multiple-frames"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

In web pages contain number of frame, selenium consider Frame is seprate window so access the content present into frame need to switch into frame. Many time we need Web structure where we have frame with in frame to navigate within frame windows Selenium provide swithTo() method.

## Frame example
    <iframe "id="iframe_Login1">
    
        <iframe "id="iframe_Login2">
      
            <iframe "id="iframe_Login3">
    
            </iframe>
    
        </iframe>
    
    </iframe>

To switch into frame in selenium use swithTo() and frame()method.

driver.switchTo().frame(iframe_Login1);
driver.switchTo().frame(iframe_Login2);
driver.switchTo().frame(iframe_Login3);

To switch back we can use parentFrame() and defaultContest();

parentFrame() : Change focus to the parent context. If the current context is the top level browsing context, the context remains unchanged.

    driver.switchTo().parentFrame();

defaultContent() :
        Selects either the first frame on the page, or the main document when a page contains iframes.

     driver.switchTo().defaultContent();



