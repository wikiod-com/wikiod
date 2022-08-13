---
title: "First project in selenium with Java"
slug: "first-project-in-selenium-with-java"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

This is an introduction to Selenium, using Java. While we don't expect you to know anything regarding Selenium, you have to have prior Java knowledge to follow this course.

**Download Links :**

 [Selenium](http://selenium-release.storage.googleapis.com/2.40/selenium-java-2.40.0.zip)

[IntelliJ IDEA](https://www.jetbrains.com/idea/download/)

 [ChromeDriver](https://chromedriver.storage.googleapis.com/index.html?path=2.29/)

 [JDK 8](http://www.oracle.com/technetwork/java/javas)

## Getting Elements in Selenium
Every Html-Element in Selenium is called a `WebElement`. For example, a `p` tag would be a `WebElement`, an `a` tag would be a `WebElement`, etc. Consider the following html Structure:

    <a id="link1" href="https://www.google.com">google</a>
    <p class="p1">
    This is a paragraph
    </p>
Now, if we wanted to get the `a` tag, we could do

    WebElement link = driver.findElement(By.id("link1"));
Now, we can click on this, by 

    link.click();
Lets take another example. If we wanted the text of the `p` tag, *ie*, "*This is a paragraph*", we can do

    WebElement p = driver.findElement(By.className("p1"));
    System.out.println(p.getText());
We can also get Elements by tags, like

    WebElement tag = driver.findElement(By.tagName("a"));

## Setting up IntelliJ Idea for Selenium
Prerequisites: 

 1. Java is installed
2. Selenium is extracted in a folder (Contains 2 files, and 1 folder)

Follow these steps to set up IntelliJ Idea for Selenium.

1. Click On **"New Project"**.
2. Choose Java < "Hello World" Application 
3. Type the name of the Project, and create it.

Your Screen should look something like this
[![enter image description here][1]][1]

Now, go to 

    File < Project Structure < Modules < Dependencies

There, click on the green plus(`+`) icon, and choose Library. Then navigate to the extracted Selenium folder, and add "*selenium-java 2.4.0.jar*". After adding this, click on the green plus(`+`) icon again, and now choose *"Directory".* This time, locate the *libs* folder of Selenium, and click on ok, while selecting it.

At the end, your Project Structure should look like this[![enter image description here][2]][2]

Now, click on OK, and you're all set.

  [1]: https://i.stack.imgur.com/aJ1FW.png
  [2]: https://i.stack.imgur.com/k7Ric.png

## Setting up ChromeDriver
Prerequisites : ChromeDriver is downloaded

Copy the following code into your class.

    public static void main(String[] args) {
        System.setProperty("webdriver.chrome.driver", "path of the exe file\\chromedriver.exe");
    }
If you're using linux, give the path to the ChromeDriver Binary.

## Opening up a Website using Selenium
We use the `get` method to go to a website. For Example, this would open google

    public static void main(String[] args) throws InterruptedException {
        System.setProperty("webdriver.chrome.driver", "path of the exe file\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();
        driver.get("https:www.google.com");
        Thread.sleep(3000); //wait for 3 seconds
        driver.quit();      //close Chrome
    }
`driver.quit()` closes the Browser. To create a delay, we use `Thread.sleep(3000)`.

## Working Example in Selenium
Now that we know the basics of Selenium, we can make our own project. For this example, we'll be making a program, which finds the Newest questions on stack-overflow. 

We start easy, lets open stack-overflow.

    public static void main(String[] args) throws InterruptedException {
        System.setProperty("webdriver.chrome.driver", "path of the exe file\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();
        driver.get("https:stackoverflow.com");
        Thread.sleep(3000);
        driver.quit();
    }
Now, if you look at the source of the page, you find that all questions, are `a` tags, with an className of `question-hyperlink`. However, since there are multiple questions, we use a `List` of `WebElement`, instead of `WebElement`. Thus, we can do 

    public static void main(String[] args) throws InterruptedException {
        System.setProperty("webdriver.chrome.driver", "path to chromedriver\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();
        driver.get("https:stackoverflow.com");
        List<WebElement> list = driver.findElements(By.className("question-hyperlink"));
    }
Now, we need to get the `href` attribute of the `a` tag, which has the link of the question. To do this, we can use `getAttribute("href")` on the each `WebElement`, like

    public static void main(String[] args) throws InterruptedException {
        System.setProperty("webdriver.chrome.driver", "path to chromedriver\\chromedriver.exe");
        WebDriver driver = new ChromeDriver();
        driver.get("https:stackoverflow.com");
        List<WebElement> list = driver.findElements(By.className("question-hyperlink"));
        System.out.println(list.size());
        list.forEach(e->System.out.println(e.getAttribute("href")));
        driver.quit();
    }
This prints out the links of the top-questions on Stack-overflow.

## Getting Attributes of WebElements in Selenium
To get the attribute of a `WebElement`, we use `getAttribute`  on that `WebElement`. For example, consider the following html tag

    <a id="click" href="https://www.google.com">
We can find the Element's `href` attribute by 

    WebElement e = driver.findElement(By.id("click"));
    System.out.println(e.getAttribute("href")); //prints https://www.google.com

