---
title: "Exceptions in Selenium-WebDriver"
slug: "exceptions-in-selenium-webdriver"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

There are a number of exceptions that can be thrown while using a webdriver. The examples below are meant to give an idea of what they mean.

## Python Exceptions

[Selenium Exception Documentation][1]<br><br>
**ElementNotInteractableException:** Thrown when an element is present in the DOM but interactions with that element will hit another element due to paint order
- **ElementNotSelectableException:** Thrown when trying to select an unselectable element. Examples of unselectable elements:
    * script
- **ElementNotVisibleException:** Thrown when an element is present on the DOM, but it is not visible, and so is not able to be interacted with. Most commonly encountered when trying to click or read text of an element that is hidden from view.
- **ErrorInResponseException:** Thrown when an error has occurred on the server side. This may happen when communicating with the firefox extension or the remote driver server.
- **ImeActivationFailedException:** Thrown when activating an IME engine has failed.
- **ImeNotAvailableException:** Thrown when IME support is not available. This exception is thrown for every IME-related method call if IME support is not available on the machine.
- **InvalidArgumentException:** The arguments passed to a command are either invalid or malformed.
- **InvalidCookieDomainException:** Thrown when attempting to add a cookie under a different domain than the current URL.
- **InvalidElementStateException:** Thrown when an action would result in an invalid state for an element. Subclasses:
    * ElementNotInteractableException
    * ElementNotSelectableException
    * ElementNotVisibleException
- **InvalidSelectorException:** Thrown when the selector which is used to find an element does not return a WebElement. Currently this only happens when the selector is an xpath expression and it is either syntactically invalid (i.e. it is not a xpath expression) or the expression does not select WebElements (e.g. “count(//input)”).
- **InvalidSwitchToTargetException:** Thrown when frame or window target to be switched doesn’t exist.
- **MoveTargetOutOfBoundsException:** Thrown when the target provided to the ActionsChains move() method is invalid, i.e. out of document.
- **NoAlertPresentException:** Thrown when switching to no presented alert. This can be caused by calling an operation on the Alert() class when an alert is not yet on the screen.
- **NoSuchAttributeException:** Thrown when the attribute of element could not be found. You may want to check if the attribute exists in the particular browser you are testing against. Some browsers may have different property names for the same property. (IE8’s .innerText vs. Firefox .textContent)
- **NoSuchElementException:** Thrown when element could not be found. If you encounter this exception, you may want to check the following:
    * Check your selector used in your find_by...
    * Element may not yet be on the screen at the time of the find operation, (webpage is still loading) see selenium.webdriver.support.wait.WebDriverWait() for how to write a wait wrapper to wait for an element to appear.
- **NoSuchFrameException:** Thrown when frame target to be switched doesn’t exist.
- **NoSuchWindowException:** Thrown when window target to be switched doesn’t exist. To find the current set of active window handles, you can get a list of the active window handles in the following way:<br>`print driver.window_handles`
- **RemoteDriverServerException:**
- **StaleElementReferenceException:** Thrown when a reference to an element is now “stale”. Stale means the element no longer appears on the DOM of the page. Possible causes of StaleElementReferenceException include, but not limited to:
    * You are no longer on the same page, or the page may have refreshed since the element was located.
    * The element may have been removed and re-added to the screen, since it was located. Such as an element being relocated. This can happen typically with a javascript framework when values are updated and the node is rebuilt.
    * Element may have been inside an iframe or another context which was refreshed.
- **TimeoutException:** Thrown when a command does not complete in enough time.
- **UnableToSetCookieException:** Thrown when a driver fails to set a cookie.
- **UnexpectedAlertPresentException:** Thrown when an unexpected alert is appeared. Usually raised when when an expected modal is blocking webdriver form executing any more commands.
- **UnexpectedTagNameException:** Thrown when a support class did not get an expected web element.
- **WebDriverException:** Base webdriver exception. All webdriver exceptions either use WebDriverException or InvalidStateException as the parent class.


  [1]: https://seleniumhq.github.io/selenium/docs/api/py/common/selenium.common.exceptions.html

