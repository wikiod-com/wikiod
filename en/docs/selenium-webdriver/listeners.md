---
title: "Listeners"
slug: "listeners"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## EventFiringWebDriver
Using the [EventFiringWebDriver][1]. You can attach [WebDriverEventListener][2] to it and override methodes, ie the onException method:

    EventFiringWebDriver driver = new EventFiringWebDriver(new FirefoxDriver());
    WebDriverEventListener listener = new AbstractWebDriverEventListener() {
        @Override
        public void onException(Throwable t, WebDriver driver) {
            // Take action
        }
    };
    driver.register(listener);


  [1]: https://seleniumhq.github.io/selenium/docs/api/java/org/openqa/selenium/support/events/EventFiringWebDriver.html
  [2]: https://seleniumhq.github.io/selenium/docs/api/java/org/openqa/selenium/support/events/WebDriverEventListener.html

## JUnit

If you are using JUnit to execute, you can extend the `TestWatcher` class:

    public class TestRules extends TestWatcher {
    
        @Override
        protected void failed(Throwable e, Description description) {
            // This will be called whenever a test fails.
        }
    
So in your test class you can simply call it:
    
    public class testClass{
    
    @Rule
    public TestRules testRules = new TestRules();
    
    @Test
    public void doTestSomething() throws Exception{
        // If the test fails for any reason, it will be caught be testrules.
    }

