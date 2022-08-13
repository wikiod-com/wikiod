---
title: "Interacting with the Browser Window(s)"
slug: "interacting-with-the-browser-windows"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Managing the active window
C#
==

*Maximizing the window*

    driver.Manage().Window.Maximize();

This is fairly straightforward, ensures that our currently active window is maximized.

*Position of the window*

    driver.Manage().Window.Position = new System.Drawing.Point(1, 1);

Here we essentially move the currently active window to a new position. In the `Point` object we provide `x` and `y` co-ordinates; these are then used as offsets from the top-left corner of the screen to determine where the window should be placed. Note that you can also store the window position in a variable:

    System.Drawing.Point windowPosition = driver.Manage().Window.Position;

*Size of the window*

Setting and getting the window size uses the same syntax as the position:

    driver.Manage().Window.Size = new System.Drawing.Size(100, 200);
    System.Drawing.Size windowSize = driver.Manage().Window.Size;

*URL of the window*

We can obtain the current URL of the active window:

    string url = driver.Url;

We can also set the URL for the active window, which will make the driver navigate to the new value:

    driver.Url = "http://stackoverflow.com/";

*Window handles*

We can obtain the handle for the current window:

    string handle = driver.CurrentWindowHandle;

And we can obtain the handles for all open windows:

    IList<String> handles = driver.WindowHandles;

Python
==


*Maximizing the window*

    driver.maximize_window()
    
*Get position of the window*

    driver.get_window_position() # returns {'y', 'x'} coordinates

*Set position of the window*
  

    driver.set_window_position(x, y) # pass 'x' and 'y' coordinates as arguments

*Get size of the window*

    driver.get_window_size() # returns {'width', 'height'} values

*Set size of the window*

    driver.set_window_size(width, height) # pass 'width' and 'height' values as arguments

*Current page title*

    driver.title

*Current URL*

    driver.current_url

*Window handles*

    driver.current_window_handle

*List of currently opened windows*

    driver.window_handles


## Closing the current browser window
Switch to the new opened tab. Close the current windows(In this case the new Tab). Switch back to first window.

**PROTRACTOR:**

    browser.getAllWindowHandles().then(function (handles) {
        browser.driver.switchTo().window(handles[1]);
        browser.driver.close();
        browser.driver.switchTo().window(handles[0]);
    });

**JAVA Selenium:**

            Set<String> handlesSet = driver.getWindowHandles();
            List<String> handlesList = new ArrayList<String>(handlesSet);
            driver.switchTo().window(handlesList.get(1));
            driver.close();
            driver.switchTo().window(handlesList.get(0));



## Handle multiple windows
Python
==

Most commonly used scenario:

 1. *open page in new window* 
 2. *switch to it* 
 3. *do something*
 4. *close it* 
 5. *switch back to parent window*


    # Open "Google" page in parent window
    driver.get("https://google.com")
 
    driver.title # 'Google'

    # Get parent window
    parent_window = driver.current_window_handle 

    # Open "Bing" page in child window
    driver.execute_script("window.open('https://bing.com')") 

    # Get list of all windows currently opened (parent + child)
    all_windows = driver.window_handles 

    # Get child window
    child_window = [window for window in all_windows if window != parent_window][0] 

    # Switch to child window
    driver.switch_to.window(child_window) 

    driver.title # 'Bing'

    # Close child window
    driver.close() 

    # Switch back to parent window
    driver.switch_to.window(parent_window) 

    driver.title # 'Google'

